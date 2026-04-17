{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.Http
  ( runHttp
  , HttpConfig (..)
  , defaultHttpConfig
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Network.HTTP.Types
import Network.Wai hiding (Request)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)

import MCP.JsonRpc
import MCP.Server

data HttpConfig = HttpConfig
  { httpPort :: !Int
  , httpPath :: !BS.ByteString
  } deriving (Show, Eq)

defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpPath = "/mcp"
  }

data Session = Session
  { sessionServer    :: !McpServer
  , sessionSseChans  :: !(TVar [TChan Message])
  }

newSession :: McpServer -> IO Session
newSession srv = Session srv <$> newTVarIO []

runHttp :: HttpConfig -> McpServer -> IO ()
runHttp cfg srv = do
  sessionsRef <- newIORef Map.empty
  run (httpPort cfg) (app cfg srv sessionsRef)

app :: HttpConfig -> McpServer -> IORef (Map Text Session) -> Wai.Application
app cfg srv sessionsRef req respond
  | rawPathInfo req /= httpPath cfg =
      respond $ responseLBS status404 [] "Not found"
  | requestMethod req == "POST" =
      handlePost srv sessionsRef req respond
  | requestMethod req == "GET" =
      handleGet sessionsRef req respond
  | requestMethod req == "DELETE" =
      handleDelete sessionsRef req respond
  | otherwise =
      respond $ responseLBS status405 [] "Method not allowed"

handlePost :: McpServer -> IORef (Map Text Session) -> Wai.Application
handlePost srv sessionsRef req respond = do
  body <- consumeBody req
  case eitherDecodeStrict' body of
    Left _err ->
      respond $ responseLBS status400 [jsonContentType] $
        encode $ MkResponse (IdInt 0) $ Left $
          MkResponseError ParseError "Parse error" Nothing
    Right msg -> case msg of
      MsgNotification _ -> do
        let sessionId = getSessionId req
        sessions <- readIORef sessionsRef
        case sessionId >>= flip Map.lookup sessions of
          Just sess -> do
            result <- handleMessage (sessionServer sess) msg
            case result of
              Just response -> broadcastToSse sess response
              Nothing       -> pure ()
          Nothing -> do
            _ <- handleMessage srv msg
            pure ()
        respond $ responseLBS status202 [] ""

      MsgRequest request -> do
        let sessionId = getSessionId req
        sessions <- readIORef sessionsRef
        result <- case sessionId >>= flip Map.lookup sessions of
          Just sess -> handleMessage (sessionServer sess) msg
          Nothing   -> handleMessage srv msg

        case result of
          Nothing -> respond $ responseLBS status202 [] ""
          Just response -> do
            extraHeaders <- case reqMethod request of
              "initialize" -> case response of
                MsgResponse (MkResponse _ (Right _)) -> do
                  sid <- T.pack . UUID.toString <$> nextRandom
                  sess <- newSession srv
                  modifyIORef' sessionsRef (Map.insert sid sess)
                  pure [("mcp-session-id", TE.encodeUtf8 sid)]
                _ -> pure []
              _ -> pure []
            respond $ responseLBS status200 (jsonContentType : extraHeaders) (encode response)

      MsgResponse _ ->
        respond $ responseLBS status202 [] ""

handleGet :: IORef (Map Text Session) -> Wai.Application
handleGet sessionsRef req respond = do
  let sessionId = getSessionId req
  sessions <- readIORef sessionsRef
  case sessionId >>= flip Map.lookup sessions of
    Nothing -> respond $ responseLBS status404 [] "Session not found"
    Just sess -> do
      chan <- atomically $ do
        c <- newTChan
        modifyTVar' (sessionSseChans sess) (c :)
        pure c
      respond $ responseStream status200 sseHeaders $ \write flush -> do
        let loop = do
              mmsg <- atomically $ readTChan chan
              let eventData = LBS.toStrict (encode mmsg)
              write (Builder.byteString "data: " <> Builder.byteString eventData <> Builder.byteString "\n\n")
              flush
              loop
        loop `catch` (\(_ :: SomeException) -> pure ())

handleDelete :: IORef (Map Text Session) -> Wai.Application
handleDelete sessionsRef req respond = do
  let sessionId = getSessionId req
  case sessionId of
    Nothing -> respond $ responseLBS status400 [] "Missing session id"
    Just sid -> do
      modifyIORef' sessionsRef (Map.delete sid)
      respond $ responseLBS status200 [] ""

broadcastToSse :: Session -> Message -> IO ()
broadcastToSse sess msg = do
  chans <- readTVarIO (sessionSseChans sess)
  mapM_ (\c -> atomically $ writeTChan c msg) chans

getSessionId :: Wai.Request -> Maybe Text
getSessionId waiReq = TE.decodeUtf8 <$> Prelude.lookup "mcp-session-id" (requestHeaders waiReq)

consumeBody :: Wai.Request -> IO BS.ByteString
consumeBody req = do
  chunks <- collectChunks
  pure $ BS.concat chunks
  where
    collectChunks = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then pure []
        else (chunk :) <$> collectChunks

jsonContentType :: Header
jsonContentType = ("Content-Type", "application/json")

sseHeaders :: ResponseHeaders
sseHeaders =
  [ ("Content-Type", "text/event-stream")
  , ("Cache-Control", "no-cache")
  , ("Connection", "keep-alive")
  ]
