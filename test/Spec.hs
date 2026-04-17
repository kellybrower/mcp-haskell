{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)

import MCP.JsonRpc
import MCP.Server
import MCP.Types

main :: IO ()
main = do
  putStrLn "Running mcp-haskell tests..."
  results <- sequence
    [ testJsonRpcRequest
    , testJsonRpcNotification
    , testJsonRpcResponse
    , testJsonRpcErrorResponse
    , testRoundtripTool
    , testRoundtripResource
    , testRoundtripContent
    , testInitializeParams
    , testRegisterTool
    , testRegisterResource
    , testRegisterPrompt
    , testInitializeHandshake
    , testToolsListAndCall
    , testResourcesListAndRead
    , testPromptsListAndGet
    , testPing
    , testUnknownMethod
    ]
  if and results
    then putStrLn "All tests passed." >> exitSuccess
    else putStrLn "Some tests FAILED." >> exitFailure

assert :: String -> Bool -> IO Bool
assert name True  = putStrLn ("  PASS: " ++ name) >> pure True
assert name False = putStrLn ("  FAIL: " ++ name) >> pure False

roundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => String -> a -> IO Bool
roundtrip name val = assert name (decode (encode val) == Just val)

-- JSON-RPC roundtrip tests

testJsonRpcRequest :: IO Bool
testJsonRpcRequest = do
  let req = MkRequest (IdInt 1) "initialize" (Just (object ["foo" .= ("bar" :: String)]))
  assert "Request encodes/decodes" (decode (encode req) == Just req)

testJsonRpcNotification :: IO Bool
testJsonRpcNotification = do
  let notif = MkNotification "notifications/initialized" Nothing
  assert "Notification encodes/decodes" (decode (encode notif) == Just notif)

testJsonRpcResponse :: IO Bool
testJsonRpcResponse = do
  let resp = MkResponse (IdInt 1) (Right (object ["result" .= ("ok" :: String)]))
  assert "Response (success) encodes/decodes" (decode (encode resp) == Just resp)

testJsonRpcErrorResponse :: IO Bool
testJsonRpcErrorResponse = do
  let resp = MkResponse (IdInt 1) (Left (MkResponseError MethodNotFound "Not found" Nothing))
  assert "Response (error) encodes/decodes" (decode (encode resp) == Just resp)

-- MCP type roundtrip tests

testRoundtripTool :: IO Bool
testRoundtripTool = roundtrip "Tool roundtrip" Tool
  { toolName = "test"
  , toolDescription = "A test tool"
  , toolInputSchema = object ["type" .= ("object" :: String)]
  , toolOutputSchema = Nothing
  , toolAnnotations = Nothing
  }

testRoundtripResource :: IO Bool
testRoundtripResource = roundtrip "Resource roundtrip" Resource
  { resUri = "file:///test.txt"
  , resName = "test"
  , resTitle = Nothing
  , resDescription = Just "A test resource"
  , resMimeType = Just "text/plain"
  , resSize = Nothing
  , resAnnotations = Nothing
  }

testRoundtripContent :: IO Bool
testRoundtripContent = roundtrip "TextContent roundtrip" $
  TextContent "hello" Nothing

testInitializeParams :: IO Bool
testInitializeParams = roundtrip "InitializeParams roundtrip" InitializeParams
  { ipProtocolVersion = protocolVersion
  , ipCapabilities = defaultClientCapabilities
  , ipClientInfo = ImplInfo "test-client" "1.0" Nothing
  }

-- Registration tests

testRegisterTool :: IO Bool
testRegisterTool = do
  srv <- mkServer defaultServerConfig
  let tool = Tool "test-tool" "desc" (object ["type" .= ("object" :: String)]) Nothing Nothing
  registerTool srv tool (\_ -> pure $ ToolResult [] False Nothing)
  ts <- readTVarIO (tools srv)
  assert "registerTool adds tool" (Map.member "test-tool" ts)

testRegisterResource :: IO Bool
testRegisterResource = do
  srv <- mkServer defaultServerConfig
  let res = Resource "test://res" "test-res" Nothing Nothing Nothing Nothing Nothing
  registerResource srv res (\_ -> pure [])
  rs <- readTVarIO (resources srv)
  assert "registerResource adds resource" (Map.member "test://res" rs)

testRegisterPrompt :: IO Bool
testRegisterPrompt = do
  srv <- mkServer defaultServerConfig
  let prompt = Prompt "test-prompt" Nothing Nothing Nothing
  registerPrompt srv prompt (\_ -> pure [])
  ps <- readTVarIO (prompts srv)
  assert "registerPrompt adds prompt" (Map.member "test-prompt" ps)

-- Server handler tests

mkTestServer :: IO McpServer
mkTestServer = do
  srv <- mkServer defaultServerConfig
  registerTool srv
    (Tool "echo" "echoes" (object ["type" .= ("object" :: String)]) Nothing Nothing)
    (\_ -> pure $ ToolResult [TextContent "echoed" Nothing] False Nothing)
  registerResource srv
    (Resource "test://hello" "hello" Nothing Nothing (Just "text/plain") Nothing Nothing)
    (\_ -> pure [ResourceContents "test://hello" (Just "text/plain") (Just "Hello!") Nothing Nothing])
  registerPrompt srv
    (Prompt "greet" Nothing (Just "Greeting prompt") Nothing)
    (\_ -> pure [PromptMessage RoleUser (TextContent "Hello!" Nothing)])
  pure srv

sendRequest :: McpServer -> Int -> Text -> Maybe Value -> IO (Maybe Message)
sendRequest srv n method params =
  handleMessage srv (MsgRequest (MkRequest (IdInt n) method params))

getResult :: Maybe Message -> Maybe Value
getResult (Just (MsgResponse (MkResponse _ (Right v)))) = Just v
getResult _ = Nothing

getError :: Maybe Message -> Maybe ResponseError
getError (Just (MsgResponse (MkResponse _ (Left e)))) = Just e
getError _ = Nothing

testInitializeHandshake :: IO Bool
testInitializeHandshake = do
  srv <- mkTestServer
  resp <- sendRequest srv 1 "initialize" $ Just $ object
    [ "protocolVersion" .= protocolVersion
    , "capabilities" .= object []
    , "clientInfo" .= object ["name" .= ("test" :: String), "version" .= ("1.0" :: String)]
    ]
  case getResult resp of
    Just val -> case fromJSON val of
      Success (ir :: InitializeResult) ->
        assert "Initialize returns correct version" (irProtocolVersion ir == protocolVersion)
      Error _ -> assert "Initialize parses result" False
    Nothing -> assert "Initialize returns result" False

testToolsListAndCall :: IO Bool
testToolsListAndCall = do
  srv <- mkTestServer
  listResp <- sendRequest srv 1 "tools/list" Nothing
  callResp <- sendRequest srv 2 "tools/call" $ Just $ object
    [ "name" .= ("echo" :: String)
    , "arguments" .= object ["message" .= ("hi" :: String)]
    ]
  let listOk = case getResult listResp >>= parseMaybe (withObject "r" (.: "tools")) of
        Just (ts :: [Value]) -> length ts == 1
        Nothing              -> False
  let callOk = case getResult callResp >>= parseMaybe (withObject "r" (.: "isError")) of
        Just (False :: Bool) -> True
        _                    -> False
  r1 <- assert "tools/list returns 1 tool" listOk
  r2 <- assert "tools/call succeeds" callOk
  pure (r1 && r2)

testResourcesListAndRead :: IO Bool
testResourcesListAndRead = do
  srv <- mkTestServer
  listResp <- sendRequest srv 1 "resources/list" Nothing
  readResp <- sendRequest srv 2 "resources/read" $ Just $ object
    [ "uri" .= ("test://hello" :: String) ]
  let listOk = case getResult listResp >>= parseMaybe (withObject "r" (.: "resources")) of
        Just (rs :: [Value]) -> length rs == 1
        Nothing              -> False
  let readOk = case getResult readResp >>= parseMaybe (withObject "r" (.: "contents")) of
        Just (cs :: [Value]) -> length cs == 1
        Nothing              -> False
  r1 <- assert "resources/list returns 1 resource" listOk
  r2 <- assert "resources/read returns contents" readOk
  pure (r1 && r2)

testPromptsListAndGet :: IO Bool
testPromptsListAndGet = do
  srv <- mkTestServer
  listResp <- sendRequest srv 1 "prompts/list" Nothing
  getResp <- sendRequest srv 2 "prompts/get" $ Just $ object
    [ "name" .= ("greet" :: String) ]
  let listOk = case getResult listResp >>= parseMaybe (withObject "r" (.: "prompts")) of
        Just (ps :: [Value]) -> length ps == 1
        Nothing              -> False
  let getOk = case getResult getResp >>= parseMaybe (withObject "r" (.: "messages")) of
        Just (ms :: [Value]) -> length ms == 1
        Nothing              -> False
  r1 <- assert "prompts/list returns 1 prompt" listOk
  r2 <- assert "prompts/get returns messages" getOk
  pure (r1 && r2)

testPing :: IO Bool
testPing = do
  srv <- mkTestServer
  resp <- sendRequest srv 1 "ping" Nothing
  assert "ping returns empty object" (getResult resp == Just (object []))

testUnknownMethod :: IO Bool
testUnknownMethod = do
  srv <- mkTestServer
  resp <- sendRequest srv 1 "nonexistent/method" Nothing
  let isMethodNotFound = case getError resp of
        Just e  -> errCode e == MethodNotFound
        Nothing -> False
  assert "Unknown method returns MethodNotFound" isMethodNotFound
