{-# LANGUAGE OverloadedStrings #-}

module MCP.Server
  ( McpServer (..)
  , ToolHandler
  , ResourceHandler
  , PromptHandler
  , ServerConfig (..)
  , defaultServerConfig
  , mkServer
  , handleMessage
  , registerTool
  , registerResource
  , registerPrompt
  ) where

import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)

import MCP.JsonRpc
import MCP.Types

type ToolHandler     = Value -> IO ToolResult
type ResourceHandler = Text -> IO [ResourceContents]
type PromptHandler   = Maybe Value -> IO [PromptMessage]

data ServerConfig = ServerConfig
  { serverName    :: !Text
  , serverVersion :: !Text
  , serverTitle   :: !(Maybe Text)
  , instructions  :: !(Maybe Text)
  } deriving (Show, Eq)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverName    = "mcp-haskell"
  , serverVersion = "0.1.0"
  , serverTitle   = Nothing
  , instructions  = Nothing
  }

data McpServer = McpServer
  { config       :: !ServerConfig
  , tools        :: !(TVar (Map Text (Tool, ToolHandler)))
  , resources    :: !(TVar (Map Text (Resource, ResourceHandler)))
  , prompts      :: !(TVar (Map Text (Prompt, PromptHandler)))
  , initialized  :: !(TVar Bool)
  }

mkServer :: ServerConfig -> IO McpServer
mkServer cfg = McpServer cfg
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO False

registerTool :: McpServer -> Tool -> ToolHandler -> IO ()
registerTool srv tool handler =
  atomically $ modifyTVar' (tools srv) $
    Map.insert (toolName tool) (tool, handler)

registerResource :: McpServer -> Resource -> ResourceHandler -> IO ()
registerResource srv res handler =
  atomically $ modifyTVar' (resources srv) $
    Map.insert (resUri res) (res, handler)

registerPrompt :: McpServer -> Prompt -> PromptHandler -> IO ()
registerPrompt srv prompt handler =
  atomically $ modifyTVar' (prompts srv) $
    Map.insert (promptName prompt) (prompt, handler)

handleMessage :: McpServer -> Message -> IO (Maybe Message)
handleMessage srv (MsgRequest req) = Just . MsgResponse <$> handleRequest srv req
handleMessage srv (MsgNotification notif) = do
  handleNotification srv notif
  pure Nothing
handleMessage _ (MsgResponse _) = pure Nothing

handleRequest :: McpServer -> Request -> IO Response
handleRequest srv req = case reqMethod req of
  "initialize"              -> handleInitialize srv req
  "tools/list"              -> handleToolsList srv req
  "tools/call"              -> handleToolsCall srv req
  "resources/list"          -> handleResourcesList srv req
  "resources/read"          -> handleResourcesRead srv req
  "resources/templates/list"-> handleResourceTemplatesList srv req
  "prompts/list"            -> handlePromptsList srv req
  "prompts/get"             -> handlePromptsGet srv req
  "ping"                    -> pure $ MkResponse (reqId req) (Right (object []))
  _                         -> pure $ errResponse (reqId req) MethodNotFound "Method not found"

handleNotification :: McpServer -> Notification -> IO ()
handleNotification srv notif = case notifMethod notif of
  "notifications/initialized" -> atomically $ writeTVar (initialized srv) True
  _                           -> pure ()

handleInitialize :: McpServer -> Request -> IO Response
handleInitialize srv req = do
  let caps = buildCapabilities srv
  let result = InitializeResult
        { irProtocolVersion = protocolVersion
        , irCapabilities    = caps
        , irServerInfo      = ImplInfo (serverName (config srv)) (serverVersion (config srv)) (serverTitle (config srv))
        , irInstructions    = instructions (config srv)
        }
  pure $ MkResponse (reqId req) (Right (toJSON result))

buildCapabilities :: McpServer -> ServerCapabilities
buildCapabilities _srv = defaultServerCapabilities
  { scTools     = Just (object ["listChanged" .= False])
  , scResources = Just (object ["subscribe" .= False, "listChanged" .= False])
  , scPrompts   = Just (object ["listChanged" .= False])
  }

handleToolsList :: McpServer -> Request -> IO Response
handleToolsList srv req = do
  ts <- readTVarIO (tools srv)
  let toolList = map fst (Map.elems ts)
  pure $ MkResponse (reqId req) $ Right $ object
    [ "tools" .= toolList ]

handleToolsCall :: McpServer -> Request -> IO Response
handleToolsCall srv req = case reqParams req of
  Nothing -> pure $ errResponse (reqId req) InvalidParams "Missing params"
  Just params -> case fromJSON params of
    Error msg   -> pure $ errResponse (reqId req) InvalidParams (pack msg)
    Success tcp -> do
      ts <- readTVarIO (tools srv)
      case Map.lookup (tcName tcp) ts of
        Nothing -> pure $ errResponse (reqId req) InvalidParams ("Unknown tool: " <> tcName tcp)
        Just (_, handler) -> do
          result <- handler (tcArguments tcp)
          pure $ MkResponse (reqId req) (Right (toJSON result))

handleResourcesList :: McpServer -> Request -> IO Response
handleResourcesList srv req = do
  rs <- readTVarIO (resources srv)
  let resList = map fst (Map.elems rs)
  pure $ MkResponse (reqId req) $ Right $ object
    [ "resources" .= resList ]

handleResourcesRead :: McpServer -> Request -> IO Response
handleResourcesRead srv req = case reqParams req >>= parseMaybe (withObject "params" (.: "uri")) of
  Nothing  -> pure $ errResponse (reqId req) InvalidParams "Missing uri"
  Just uri -> do
    rs <- readTVarIO (resources srv)
    case Map.lookup uri rs of
      Nothing -> pure $ errResponse (reqId req) ResourceNotFound ("Resource not found: " <> uri)
      Just (_, handler) -> do
        contents <- handler uri
        pure $ MkResponse (reqId req) $ Right $ object
          [ "contents" .= contents ]

handleResourceTemplatesList :: McpServer -> Request -> IO Response
handleResourceTemplatesList _srv req =
  pure $ MkResponse (reqId req) $ Right $ object
    [ "resourceTemplates" .= ([] :: [Value]) ]

handlePromptsList :: McpServer -> Request -> IO Response
handlePromptsList srv req = do
  ps <- readTVarIO (prompts srv)
  let promptList = map fst (Map.elems ps)
  pure $ MkResponse (reqId req) $ Right $ object
    [ "prompts" .= promptList ]

handlePromptsGet :: McpServer -> Request -> IO Response
handlePromptsGet srv req = case reqParams req >>= parseMaybe (withObject "params" (.: "name")) of
  Nothing   -> pure $ errResponse (reqId req) InvalidParams "Missing name"
  Just name -> do
    ps <- readTVarIO (prompts srv)
    case Map.lookup name ps of
      Nothing -> pure $ errResponse (reqId req) InvalidParams ("Unknown prompt: " <> name)
      Just (_, handler) -> do
        let args = reqParams req >>= parseMaybe (withObject "params" (.:? "arguments")) >>= id
        msgs <- handler args
        pure $ MkResponse (reqId req) $ Right $ object
          [ "messages" .= msgs ]

errResponse :: Id -> ErrorCode -> Text -> Response
errResponse rid code msg = MkResponse rid $ Left $ MkResponseError code msg Nothing
