# Changelog

## 0.1.0.0 -- 2026-04-17

* JSON-RPC 2.0 message types (Request, Response, Notification)
* MCP protocol types: Tool, Resource, Prompt, Content
* Server framework with `registerTool`, `registerResource`, `registerPrompt`
* STDIO transport with graceful EOF/shutdown handling
* Streamable HTTP transport with SSE (via warp)
* Session management for HTTP transport
* "Everything Server" reference implementation for conformance testing
* MCP specification version 2025-11-25
