# mcp-haskell

A Haskell SDK for the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP), enabling Haskell applications to serve tools, resources, and prompts to LLM hosts like Claude.

Implements MCP specification **2025-11-25**.

## Features

- **JSON-RPC 2.0** message layer with full request/response/notification types
- **Tools** — expose callable functions to LLMs
- **Resources** — serve static or dynamic data (text, binary, URIs)
- **Prompts** — provide reusable interaction templates
- **STDIO transport** — for local MCP servers (subprocess model)
- **Streamable HTTP transport** — for remote servers with SSE support
- **Server framework** — register handlers with `registerTool`, `registerResource`, `registerPrompt`

## Quick start

```bash
cabal build all
```

### Minimal server

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text)
import Prelude hiding (lookup)

import MCP.Server
import MCP.Types
import MCP.Transport.Stdio

main :: IO ()
main = do
  srv <- mkServer ServerConfig
    { serverName    = "my-server"
    , serverVersion = "0.1.0"
    , serverTitle   = Just "My MCP Server"
    , instructions  = Nothing
    }

  registerTool srv greetTool greetHandler
  runStdio srv

greetTool :: Tool
greetTool = Tool
  { toolName        = "greet"
  , toolDescription = "Greets someone by name"
  , toolInputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text) ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  , toolOutputSchema = Nothing
  , toolAnnotations  = Nothing
  }

greetHandler :: ToolHandler
greetHandler args = case args of
  Object o | Just (String name) <- lookup "name" o ->
    pure ToolResult
      { trContent = [TextContent ("Hello, " <> name <> "!") Nothing]
      , trIsError = False
      , trStructuredContent = Nothing
      }
  _ ->
    pure ToolResult
      { trContent = [TextContent "missing 'name'" Nothing]
      , trIsError = True
      , trStructuredContent = Nothing
      }
```

### Test it

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' \
  | cabal run mcp-example-echo
```

### HTTP transport

```haskell
import MCP.Transport.Http

main :: IO ()
main = do
  srv <- mkServer myConfig
  registerTool srv myTool myHandler
  runHttp defaultHttpConfig { httpPort = 8080 } srv
```

The Everything Server supports both:

```bash
cabal run mcp-everything-server              # STDIO (default)
cabal run mcp-everything-server -- --http    # HTTP on port 3000
cabal run mcp-everything-server -- --http 8080
```

## Project structure

```
src/MCP/
  JsonRpc.hs           -- JSON-RPC 2.0 message types
  Types.hs             -- MCP protocol types (Tool, Resource, Prompt, Content, etc.)
  Server.hs            -- Server framework and request dispatch
  Transport/
    Stdio.hs           -- STDIO transport
    Http.hs            -- Streamable HTTP transport with SSE
examples/
  Echo.hs              -- Minimal echo server
  Everything.hs        -- Reference implementation for conformance testing
test/
  Spec.hs              -- 20 tests (roundtrips, registration, handler dispatch)
```

## Modules

| Module | Purpose |
|---|---|
| `MCP.JsonRpc` | `Request`, `Response`, `Notification`, `Message`, error codes |
| `MCP.Types` | `Tool`, `Resource`, `Prompt`, `Content`, `Annotations`, capabilities, lifecycle types |
| `MCP.Server` | `mkServer`, `registerTool`, `registerResource`, `registerPrompt`, `handleMessage` |
| `MCP.Transport.Stdio` | `runStdio` — newline-delimited JSON-RPC on stdin/stdout |
| `MCP.Transport.Http` | `runHttp` — HTTP POST + SSE streaming with session management |

## Everything Server

The `mcp-everything-server` is a reference implementation for MCP conformance testing. It exposes:

**Tools:** `echo`, `add`, `longRunningOperation`, `sampleLLM`, `getCurrentTime`, `annotatedMessage`

**Resources:** `test://static/hello` (text), `test://static/binary` (base64), `test://dynamic/time` (live)

**Prompts:** `simple_prompt` (no args), `complex_prompt` (temperature, style)

## Tests

```bash
cabal test
```

## Roadmap

- [ ] MCP conformance test suite integration
- [ ] Resource subscription (`resources/subscribe`)
- [ ] Progress notifications for long-running tools
- [ ] Client SDK (for building MCP clients)
- [ ] Hackage release

## License

BSD-3-Clause
