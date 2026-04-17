{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Types
  ( -- * Protocol version
    protocolVersion

    -- * Implementation info
  , ImplInfo (..)
  , Icon (..)

    -- * Capabilities
  , ClientCapabilities (..)
  , ServerCapabilities (..)
  , defaultClientCapabilities
  , defaultServerCapabilities

    -- * Initialize
  , InitializeParams (..)
  , InitializeResult (..)

    -- * Tools
  , Tool (..)
  , ToolCallParams (..)
  , ToolResult (..)
  , Content (..)
  , Annotations (..)

    -- * Resources
  , Resource (..)
  , ResourceContents (..)
  , ResourceTemplate (..)

    -- * Prompts
  , Prompt (..)
  , PromptArgument (..)
  , PromptMessage (..)
  , Role (..)

    -- * Pagination
  , Cursor
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- | Current MCP protocol version.
protocolVersion :: Text
protocolVersion = "2025-11-25"

type Cursor = Text

-- | Implementation info for client or server.
data ImplInfo = ImplInfo
  { implName    :: !Text
  , implVersion :: !Text
  , implTitle   :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON ImplInfo where
  toJSON i = object $
    [ "name"    .= implName i
    , "version" .= implVersion i
    ] ++ maybe [] (\t -> ["title" .= t]) (implTitle i)

instance FromJSON ImplInfo where
  parseJSON = withObject "ImplInfo" $ \o ->
    ImplInfo
      <$> o .: "name"
      <*> o .: "version"
      <*> o .:? "title"

data Icon = Icon
  { iconSrc      :: !Text
  , iconMimeType :: !Text
  , iconSizes    :: ![Text]
  } deriving (Show, Eq)

instance ToJSON Icon where
  toJSON i = object
    [ "src"      .= iconSrc i
    , "mimeType" .= iconMimeType i
    , "sizes"    .= iconSizes i
    ]

instance FromJSON Icon where
  parseJSON = withObject "Icon" $ \o ->
    Icon <$> o .: "src" <*> o .: "mimeType" <*> o .:? "sizes" .!= []

-- | Client capabilities sent during initialization.
data ClientCapabilities = ClientCapabilities
  { ccRoots        :: !(Maybe Value)
  , ccSampling     :: !(Maybe Value)
  , ccExperimental :: !(Maybe Value)
  } deriving (Show, Eq)

defaultClientCapabilities :: ClientCapabilities
defaultClientCapabilities = ClientCapabilities Nothing Nothing Nothing

instance ToJSON ClientCapabilities where
  toJSON c = object $
    maybe [] (\v -> ["roots" .= v]) (ccRoots c)
    ++ maybe [] (\v -> ["sampling" .= v]) (ccSampling c)
    ++ maybe [] (\v -> ["experimental" .= v]) (ccExperimental c)

instance FromJSON ClientCapabilities where
  parseJSON = withObject "ClientCapabilities" $ \o ->
    ClientCapabilities
      <$> o .:? "roots"
      <*> o .:? "sampling"
      <*> o .:? "experimental"

-- | Server capabilities declared during initialization.
data ServerCapabilities = ServerCapabilities
  { scTools        :: !(Maybe Value)
  , scResources    :: !(Maybe Value)
  , scPrompts      :: !(Maybe Value)
  , scLogging      :: !(Maybe Value)
  , scExperimental :: !(Maybe Value)
  } deriving (Show, Eq)

defaultServerCapabilities :: ServerCapabilities
defaultServerCapabilities = ServerCapabilities Nothing Nothing Nothing Nothing Nothing

instance ToJSON ServerCapabilities where
  toJSON c = object $
    maybe [] (\v -> ["tools" .= v]) (scTools c)
    ++ maybe [] (\v -> ["resources" .= v]) (scResources c)
    ++ maybe [] (\v -> ["prompts" .= v]) (scPrompts c)
    ++ maybe [] (\v -> ["logging" .= v]) (scLogging c)
    ++ maybe [] (\v -> ["experimental" .= v]) (scExperimental c)

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \o ->
    ServerCapabilities
      <$> o .:? "tools"
      <*> o .:? "resources"
      <*> o .:? "prompts"
      <*> o .:? "logging"
      <*> o .:? "experimental"

-- | Parameters for the initialize request.
data InitializeParams = InitializeParams
  { ipProtocolVersion :: !Text
  , ipCapabilities    :: !ClientCapabilities
  , ipClientInfo      :: !ImplInfo
  } deriving (Show, Eq)

instance ToJSON InitializeParams where
  toJSON p = object
    [ "protocolVersion" .= ipProtocolVersion p
    , "capabilities"    .= ipCapabilities p
    , "clientInfo"      .= ipClientInfo p
    ]

instance FromJSON InitializeParams where
  parseJSON = withObject "InitializeParams" $ \o ->
    InitializeParams
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "clientInfo"

-- | Result of the initialize request.
data InitializeResult = InitializeResult
  { irProtocolVersion :: !Text
  , irCapabilities    :: !ServerCapabilities
  , irServerInfo      :: !ImplInfo
  , irInstructions    :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON InitializeResult where
  toJSON r = object $
    [ "protocolVersion" .= irProtocolVersion r
    , "capabilities"    .= irCapabilities r
    , "serverInfo"      .= irServerInfo r
    ] ++ maybe [] (\i -> ["instructions" .= i]) (irInstructions r)

instance FromJSON InitializeResult where
  parseJSON = withObject "InitializeResult" $ \o ->
    InitializeResult
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "serverInfo"
      <*> o .:? "instructions"

data Annotations = Annotations
  { annAudience     :: !(Maybe [Text])
  , annPriority     :: !(Maybe Double)
  , annLastModified :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON Annotations where
  toJSON a = object $
    maybe [] (\v -> ["audience" .= v]) (annAudience a)
    ++ maybe [] (\v -> ["priority" .= v]) (annPriority a)
    ++ maybe [] (\v -> ["lastModified" .= v]) (annLastModified a)

instance FromJSON Annotations where
  parseJSON = withObject "Annotations" $ \o ->
    Annotations
      <$> o .:? "audience"
      <*> o .:? "priority"
      <*> o .:? "lastModified"

-- | Content items returned from tool calls and prompts.
data Content
  = TextContent !Text !(Maybe Annotations)
  | ImageContent !Text !Text !(Maybe Annotations)  -- data, mimeType
  | AudioContent !Text !Text                        -- data, mimeType
  | ResourceLink !Text !Text !(Maybe Text) !(Maybe Text) !(Maybe Annotations)  -- uri, name, desc, mime
  | EmbeddedResource !ResourceContents !(Maybe Annotations)
  deriving (Show, Eq)

instance ToJSON Content where
  toJSON (TextContent t ann) = object $
    [ "type" .= ("text" :: Text), "text" .= t ]
    ++ maybe [] (\a -> ["annotations" .= a]) ann
  toJSON (ImageContent d mime ann) = object $
    [ "type" .= ("image" :: Text), "data" .= d, "mimeType" .= mime ]
    ++ maybe [] (\a -> ["annotations" .= a]) ann
  toJSON (AudioContent d mime) = object
    [ "type" .= ("audio" :: Text), "data" .= d, "mimeType" .= mime ]
  toJSON (ResourceLink uri name desc mime ann) = object $
    [ "type" .= ("resource_link" :: Text), "uri" .= uri, "name" .= name ]
    ++ maybe [] (\d -> ["description" .= d]) desc
    ++ maybe [] (\m -> ["mimeType" .= m]) mime
    ++ maybe [] (\a -> ["annotations" .= a]) ann
  toJSON (EmbeddedResource rc ann) = object $
    [ "type" .= ("resource" :: Text), "resource" .= rc ]
    ++ maybe [] (\a -> ["annotations" .= a]) ann

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text"          -> TextContent <$> o .: "text" <*> o .:? "annotations"
      "image"         -> ImageContent <$> o .: "data" <*> o .: "mimeType" <*> o .:? "annotations"
      "audio"         -> AudioContent <$> o .: "data" <*> o .: "mimeType"
      "resource_link" -> ResourceLink <$> o .: "uri" <*> o .: "name" <*> o .:? "description" <*> o .:? "mimeType" <*> o .:? "annotations"
      "resource"      -> EmbeddedResource <$> o .: "resource" <*> o .:? "annotations"
      _               -> fail $ "Unknown content type: " ++ show typ

-- | A tool definition exposed by the server.
data Tool = Tool
  { toolName         :: !Text
  , toolDescription  :: !Text
  , toolInputSchema  :: !Value
  , toolOutputSchema :: !(Maybe Value)
  , toolAnnotations  :: !(Maybe Annotations)
  } deriving (Show, Eq)

instance ToJSON Tool where
  toJSON t = object $
    [ "name"        .= toolName t
    , "description" .= toolDescription t
    , "inputSchema" .= toolInputSchema t
    ] ++ maybe [] (\v -> ["outputSchema" .= v]) (toolOutputSchema t)
      ++ maybe [] (\a -> ["annotations" .= a]) (toolAnnotations t)

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o ->
    Tool
      <$> o .: "name"
      <*> o .: "description"
      <*> o .: "inputSchema"
      <*> o .:? "outputSchema"
      <*> o .:? "annotations"

-- | Parameters for tools/call.
data ToolCallParams = ToolCallParams
  { tcName      :: !Text
  , tcArguments :: !Value
  } deriving (Show, Eq)

instance ToJSON ToolCallParams where
  toJSON p = object
    [ "name"      .= tcName p
    , "arguments" .= tcArguments p
    ]

instance FromJSON ToolCallParams where
  parseJSON = withObject "ToolCallParams" $ \o ->
    ToolCallParams <$> o .: "name" <*> o .: "arguments"

-- | Result of a tool call.
data ToolResult = ToolResult
  { trContent           :: ![Content]
  , trIsError           :: !Bool
  , trStructuredContent :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON ToolResult where
  toJSON r = object $
    [ "content" .= trContent r
    , "isError" .= trIsError r
    ] ++ maybe [] (\v -> ["structuredContent" .= v]) (trStructuredContent r)

instance FromJSON ToolResult where
  parseJSON = withObject "ToolResult" $ \o ->
    ToolResult
      <$> o .: "content"
      <*> o .: "isError"
      <*> o .:? "structuredContent"

-- | A resource definition.
data Resource = Resource
  { resUri         :: !Text
  , resName        :: !Text
  , resTitle       :: !(Maybe Text)
  , resDescription :: !(Maybe Text)
  , resMimeType    :: !(Maybe Text)
  , resSize        :: !(Maybe Int)
  , resAnnotations :: !(Maybe Annotations)
  } deriving (Show, Eq)

instance ToJSON Resource where
  toJSON r = object $
    [ "uri"  .= resUri r
    , "name" .= resName r
    ] ++ maybe [] (\v -> ["title" .= v]) (resTitle r)
      ++ maybe [] (\v -> ["description" .= v]) (resDescription r)
      ++ maybe [] (\v -> ["mimeType" .= v]) (resMimeType r)
      ++ maybe [] (\v -> ["size" .= v]) (resSize r)
      ++ maybe [] (\a -> ["annotations" .= a]) (resAnnotations r)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o ->
    Resource
      <$> o .: "uri"
      <*> o .: "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "mimeType"
      <*> o .:? "size"
      <*> o .:? "annotations"

-- | Contents returned from resources/read.
data ResourceContents = ResourceContents
  { rcUri         :: !Text
  , rcMimeType    :: !(Maybe Text)
  , rcText        :: !(Maybe Text)
  , rcBlob        :: !(Maybe Text)
  , rcAnnotations :: !(Maybe Annotations)
  } deriving (Show, Eq)

instance ToJSON ResourceContents where
  toJSON r = object $
    [ "uri" .= rcUri r ]
    ++ maybe [] (\v -> ["mimeType" .= v]) (rcMimeType r)
    ++ maybe [] (\v -> ["text" .= v]) (rcText r)
    ++ maybe [] (\v -> ["blob" .= v]) (rcBlob r)
    ++ maybe [] (\a -> ["annotations" .= a]) (rcAnnotations r)

instance FromJSON ResourceContents where
  parseJSON = withObject "ResourceContents" $ \o ->
    ResourceContents
      <$> o .: "uri"
      <*> o .:? "mimeType"
      <*> o .:? "text"
      <*> o .:? "blob"
      <*> o .:? "annotations"

-- | A resource template (URI template).
data ResourceTemplate = ResourceTemplate
  { rtUriTemplate  :: !Text
  , rtName         :: !Text
  , rtTitle        :: !(Maybe Text)
  , rtDescription  :: !(Maybe Text)
  , rtMimeType     :: !(Maybe Text)
  , rtAnnotations  :: !(Maybe Annotations)
  } deriving (Show, Eq)

instance ToJSON ResourceTemplate where
  toJSON r = object $
    [ "uriTemplate" .= rtUriTemplate r
    , "name"        .= rtName r
    ] ++ maybe [] (\v -> ["title" .= v]) (rtTitle r)
      ++ maybe [] (\v -> ["description" .= v]) (rtDescription r)
      ++ maybe [] (\v -> ["mimeType" .= v]) (rtMimeType r)
      ++ maybe [] (\a -> ["annotations" .= a]) (rtAnnotations r)

instance FromJSON ResourceTemplate where
  parseJSON = withObject "ResourceTemplate" $ \o ->
    ResourceTemplate
      <$> o .: "uriTemplate"
      <*> o .: "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "mimeType"
      <*> o .:? "annotations"

data Role = RoleUser | RoleAssistant
  deriving (Show, Eq)

instance ToJSON Role where
  toJSON RoleUser      = "user"
  toJSON RoleAssistant = "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user"      -> pure RoleUser
    "assistant" -> pure RoleAssistant
    other       -> fail $ "Unknown role: " ++ show other

-- | A prompt definition.
data Prompt = Prompt
  { promptName        :: !Text
  , promptTitle       :: !(Maybe Text)
  , promptDescription :: !(Maybe Text)
  , promptArguments   :: !(Maybe [PromptArgument])
  } deriving (Show, Eq)

instance ToJSON Prompt where
  toJSON p = object $
    [ "name" .= promptName p ]
    ++ maybe [] (\v -> ["title" .= v]) (promptTitle p)
    ++ maybe [] (\v -> ["description" .= v]) (promptDescription p)
    ++ maybe [] (\v -> ["arguments" .= v]) (promptArguments p)

instance FromJSON Prompt where
  parseJSON = withObject "Prompt" $ \o ->
    Prompt
      <$> o .: "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "arguments"

data PromptArgument = PromptArgument
  { paName        :: !Text
  , paDescription :: !(Maybe Text)
  , paRequired    :: !(Maybe Bool)
  } deriving (Show, Eq)

instance ToJSON PromptArgument where
  toJSON a = object $
    [ "name" .= paName a ]
    ++ maybe [] (\v -> ["description" .= v]) (paDescription a)
    ++ maybe [] (\v -> ["required" .= v]) (paRequired a)

instance FromJSON PromptArgument where
  parseJSON = withObject "PromptArgument" $ \o ->
    PromptArgument
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "required"

-- | A message within a prompt result.
data PromptMessage = PromptMessage
  { pmRole    :: !Role
  , pmContent :: !Content
  } deriving (Show, Eq)

instance ToJSON PromptMessage where
  toJSON m = object
    [ "role"    .= pmRole m
    , "content" .= pmContent m
    ]

instance FromJSON PromptMessage where
  parseJSON = withObject "PromptMessage" $ \o ->
    PromptMessage <$> o .: "role" <*> o .: "content"
