{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Prelude hiding (lookup)
import System.Environment (getArgs)

import MCP.Server
import MCP.Types
import MCP.Transport.Stdio
import MCP.Transport.Http

main :: IO ()
main = do
  args <- getArgs
  srv <- mkServer ServerConfig
    { serverName    = "everything-server"
    , serverVersion = "0.1.0"
    , serverTitle   = Just "MCP Everything Server (Haskell)"
    , instructions  = Just "Reference implementation for MCP conformance testing."
    }

  registerTools srv
  registerResources srv
  registerPrompts srv

  case args of
    ["--http"]       -> runHttp defaultHttpConfig srv
    ["--http", port] -> runHttp defaultHttpConfig { httpPort = read port } srv
    _                -> runStdio srv

-- Tools

registerTools :: McpServer -> IO ()
registerTools srv = do
  registerTool srv echoTool echoHandler
  registerTool srv addTool addHandler
  registerTool srv longRunningTool longRunningHandler
  registerTool srv sampleLlmTool sampleLlmHandler
  registerTool srv getTimeTool getTimeHandler
  registerTool srv annotateTool annotateHandler

echoTool :: Tool
echoTool = Tool
  { toolName        = "echo"
  , toolDescription = "Echoes the input message back"
  , toolInputSchema = object
      [ "type" .= t "object"
      , "properties" .= object
          [ "message" .= object [ "type" .= t "string", "description" .= t "Message to echo" ] ]
      , "required" .= (["message"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

echoHandler :: ToolHandler
echoHandler args = case args of
  Object o -> case lookup "message" o of
    Just (String msg) -> ok [TextContent msg Nothing]
    _                 -> err "missing 'message' argument"
  _ -> err "expected object"

addTool :: Tool
addTool = Tool
  { toolName        = "add"
  , toolDescription = "Adds two numbers"
  , toolInputSchema = object
      [ "type" .= t "object"
      , "properties" .= object
          [ "a" .= object [ "type" .= t "number", "description" .= t "First number" ]
          , "b" .= object [ "type" .= t "number", "description" .= t "Second number" ]
          ]
      , "required" .= (["a", "b"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

addHandler :: ToolHandler
addHandler args = case args of
  Object o -> case (lookup "a" o, lookup "b" o) of
    (Just (Number a), Just (Number b)) ->
      let s = a + b
       in ok [TextContent (T.pack $ show (fromRational (toRational s) :: Double)) Nothing]
    _ -> err "missing or invalid 'a'/'b' arguments"
  _ -> err "expected object"

longRunningTool :: Tool
longRunningTool = Tool
  { toolName        = "longRunningOperation"
  , toolDescription = "Simulates a long-running operation"
  , toolInputSchema = object
      [ "type" .= t "object"
      , "properties" .= object
          [ "duration" .= object [ "type" .= t "number", "description" .= t "Duration hint (ignored)" ]
          , "steps" .= object [ "type" .= t "number", "description" .= t "Number of steps (ignored)" ]
          ]
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

longRunningHandler :: ToolHandler
longRunningHandler _args = ok [TextContent "Operation complete" Nothing]

sampleLlmTool :: Tool
sampleLlmTool = Tool
  { toolName        = "sampleLLM"
  , toolDescription = "Asks the connected LLM to sample a response (stub)"
  , toolInputSchema = object
      [ "type" .= t "object"
      , "properties" .= object
          [ "prompt" .= object [ "type" .= t "string", "description" .= t "Prompt to send" ]
          , "maxTokens" .= object [ "type" .= t "number", "description" .= t "Max tokens" ]
          ]
      , "required" .= (["prompt"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

sampleLlmHandler :: ToolHandler
sampleLlmHandler args = case args of
  Object o -> case lookup "prompt" o of
    Just (String p) -> ok [TextContent ("LLM sampling not yet supported. Prompt was: " <> p) Nothing]
    _               -> err "missing 'prompt' argument"
  _ -> err "expected object"

getTimeTool :: Tool
getTimeTool = Tool
  { toolName        = "getCurrentTime"
  , toolDescription = "Returns the current UTC time"
  , toolInputSchema = object [ "type" .= t "object" ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

getTimeHandler :: ToolHandler
getTimeHandler _args = do
  now <- getCurrentTime
  let formatted = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  ok [TextContent formatted Nothing]

annotateTool :: Tool
annotateTool = Tool
  { toolName        = "annotatedMessage"
  , toolDescription = "Returns a message with annotations"
  , toolInputSchema = object
      [ "type" .= t "object"
      , "properties" .= object
          [ "message" .= object [ "type" .= t "string" ]
          , "audience" .= object [ "type" .= t "string", "description" .= t "user, assistant, or both" ]
          ]
      , "required" .= (["message"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

annotateHandler :: ToolHandler
annotateHandler args = case args of
  Object o -> case lookup "message" o of
    Just (String msg) -> do
      let aud = case lookup "audience" o of
            Just (String "user")      -> Just ["user"]
            Just (String "assistant") -> Just ["assistant"]
            _                         -> Just ["user", "assistant"]
      let ann = Annotations { annAudience = aud, annPriority = Just 1.0, annLastModified = Nothing }
      ok [TextContent msg (Just ann)]
    _ -> err "missing 'message'"
  _ -> err "expected object"

-- Resources

registerResources :: McpServer -> IO ()
registerResources srv = do
  registerResource srv textResource textResourceHandler
  registerResource srv binaryResource binaryResourceHandler
  registerResource srv dynamicResource dynamicResourceHandler

textResource :: Resource
textResource = Resource
  { resUri         = "test://static/hello"
  , resName        = "Hello World"
  , resTitle       = Just "Static Text Resource"
  , resDescription = Just "A simple static text resource"
  , resMimeType    = Just "text/plain"
  , resSize        = Nothing
  , resAnnotations = Nothing
  }

textResourceHandler :: ResourceHandler
textResourceHandler _uri = pure
  [ ResourceContents
      { rcUri         = "test://static/hello"
      , rcMimeType    = Just "text/plain"
      , rcText        = Just "Hello, World! This is a static text resource."
      , rcBlob        = Nothing
      , rcAnnotations = Nothing
      }
  ]

binaryResource :: Resource
binaryResource = Resource
  { resUri         = "test://static/binary"
  , resName        = "Binary Data"
  , resTitle       = Just "Static Binary Resource"
  , resDescription = Just "A binary resource (base64-encoded)"
  , resMimeType    = Just "application/octet-stream"
  , resSize        = Nothing
  , resAnnotations = Nothing
  }

binaryResourceHandler :: ResourceHandler
binaryResourceHandler _uri = pure
  [ ResourceContents
      { rcUri         = "test://static/binary"
      , rcMimeType    = Just "application/octet-stream"
      , rcText        = Nothing
      , rcBlob        = Just "SGVsbG8gZnJvbSBiaW5hcnkh"
      , rcAnnotations = Nothing
      }
  ]

dynamicResource :: Resource
dynamicResource = Resource
  { resUri         = "test://dynamic/time"
  , resName        = "Current Time"
  , resTitle       = Just "Dynamic Time Resource"
  , resDescription = Just "Returns the current server time"
  , resMimeType    = Just "text/plain"
  , resSize        = Nothing
  , resAnnotations = Nothing
  }

dynamicResourceHandler :: ResourceHandler
dynamicResourceHandler _uri = do
  now <- getCurrentTime
  let formatted = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  pure
    [ ResourceContents
        { rcUri         = "test://dynamic/time"
        , rcMimeType    = Just "text/plain"
        , rcText        = Just ("Current server time: " <> formatted)
        , rcBlob        = Nothing
        , rcAnnotations = Nothing
        }
    ]

-- Prompts

registerPrompts :: McpServer -> IO ()
registerPrompts srv = do
  registerPrompt srv simplePrompt simplePromptHandler
  registerPrompt srv complexPrompt complexPromptHandler

simplePrompt :: Prompt
simplePrompt = Prompt
  { promptName        = "simple_prompt"
  , promptTitle       = Just "Simple Prompt"
  , promptDescription = Just "A simple prompt with no arguments"
  , promptArguments   = Nothing
  }

simplePromptHandler :: PromptHandler
simplePromptHandler _args = pure
  [ PromptMessage RoleUser (TextContent "This is a simple prompt with no arguments." Nothing)
  ]

complexPrompt :: Prompt
complexPrompt = Prompt
  { promptName        = "complex_prompt"
  , promptTitle       = Just "Complex Prompt"
  , promptDescription = Just "A prompt with arguments"
  , promptArguments   = Just
      [ PromptArgument "temperature" (Just "Sampling temperature") (Just True)
      , PromptArgument "style" (Just "Response style (formal, casual, technical)") (Just False)
      ]
  }

complexPromptHandler :: PromptHandler
complexPromptHandler args = do
  let temp = case args of
        Just (Object o) -> case lookup "temperature" o of
          Just (String v) -> v
          _               -> "0.7"
        _ -> "0.7"
  let style = case args of
        Just (Object o) -> case lookup "style" o of
          Just (String v) -> v
          _               -> "formal"
        _ -> "formal"
  pure
    [ PromptMessage RoleUser
        (TextContent ("Generate a response with temperature=" <> temp <> " in a " <> style <> " style.") Nothing)
    , PromptMessage RoleAssistant
        (TextContent "I'll generate a response based on those parameters." Nothing)
    ]

-- Helpers

t :: Text -> Text
t = id

ok :: [Content] -> IO ToolResult
ok content = pure ToolResult
  { trContent = content
  , trIsError = False
  , trStructuredContent = Nothing
  }

err :: Text -> IO ToolResult
err msg = pure ToolResult
  { trContent = [TextContent msg Nothing]
  , trIsError = True
  , trStructuredContent = Nothing
  }
