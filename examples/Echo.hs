{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text, pack)
import Prelude hiding (lookup)

import MCP.Server
import MCP.Types
import MCP.Transport.Stdio

main :: IO ()
main = do
  srv <- mkServer ServerConfig
    { serverName    = "echo-server"
    , serverVersion = "0.1.0"
    , serverTitle   = Just "Echo MCP Server"
    , instructions  = Just "A simple echo server for testing."
    }

  registerTool srv echoTool echoHandler
  runStdio srv

echoTool :: Tool
echoTool = Tool
  { toolName        = "echo"
  , toolDescription = "Echoes the input message back"
  , toolInputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "message" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The message to echo" :: Text)
              ]
          ]
      , "required" .= (["message"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

echoHandler :: ToolHandler
echoHandler args = case args of
  Object o -> case lookup "message" o of
    Just (String t) -> pure ToolResult
      { trContent = [TextContent t Nothing]
      , trIsError = False
      , trStructuredContent = Nothing
      }
    _ -> pure ToolResult
      { trContent = [TextContent "missing 'message' argument" Nothing]
      , trIsError = True
      , trStructuredContent = Nothing
      }
  _ -> pure ToolResult
    { trContent = [TextContent ("expected object, got: " <> pack (show args)) Nothing]
    , trIsError = True
    , trStructuredContent = Nothing
    }
