{-# LANGUAGE OverloadedStrings #-}

module MCP.JsonRpc
  ( Id (..)
  , Request (..)
  , Notification (..)
  , Response (..)
  , ResponseError (..)
  , Message (..)
  , ErrorCode (..)
  , errorCodeToInt
  , errorCodeFromInt
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- | JSON-RPC 2.0 message identifier.
data Id
  = IdInt !Int
  | IdText !Text
  deriving (Show, Eq, Ord)

instance ToJSON Id where
  toJSON (IdInt n)  = toJSON n
  toJSON (IdText t) = toJSON t

instance FromJSON Id where
  parseJSON (Number n) = pure . IdInt $ truncate n
  parseJSON (String t) = pure $ IdText t
  parseJSON v          = fail $ "Invalid JSON-RPC id: " ++ show v

-- | Standard JSON-RPC 2.0 error codes plus MCP-specific ones.
data ErrorCode
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ResourceNotFound
  | CustomError !Int
  deriving (Show, Eq)

errorCodeToInt :: ErrorCode -> Int
errorCodeToInt ParseError       = -32700
errorCodeToInt InvalidRequest   = -32600
errorCodeToInt MethodNotFound   = -32601
errorCodeToInt InvalidParams    = -32602
errorCodeToInt InternalError    = -32603
errorCodeToInt ResourceNotFound = -32002
errorCodeToInt (CustomError n)  = n

errorCodeFromInt :: Int -> ErrorCode
errorCodeFromInt (-32700) = ParseError
errorCodeFromInt (-32600) = InvalidRequest
errorCodeFromInt (-32601) = MethodNotFound
errorCodeFromInt (-32602) = InvalidParams
errorCodeFromInt (-32603) = InternalError
errorCodeFromInt (-32002) = ResourceNotFound
errorCodeFromInt n        = CustomError n

-- | JSON-RPC 2.0 request (has id, expects response).
data Request = MkRequest
  { reqId     :: !Id
  , reqMethod :: !Text
  , reqParams :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON Request where
  toJSON r = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= reqId r
    , "method"  .= reqMethod r
    , "params"  .= reqParams r
    ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    checkVersion o
    MkRequest
      <$> o .: "id"
      <*> o .: "method"
      <*> o .:? "params"

-- | JSON-RPC 2.0 notification (no id, no response expected).
data Notification = MkNotification
  { notifMethod :: !Text
  , notifParams :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON Notification where
  toJSON n = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= notifMethod n
    , "params"  .= notifParams n
    ]

instance FromJSON Notification where
  parseJSON = withObject "Notification" $ \o -> do
    checkVersion o
    MkNotification
      <$> o .: "method"
      <*> o .:? "params"

-- | JSON-RPC 2.0 error object.
data ResponseError = MkResponseError
  { errCode    :: !ErrorCode
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON ResponseError where
  toJSON e = object
    [ "code"    .= errorCodeToInt (errCode e)
    , "message" .= errMessage e
    , "data"    .= errData e
    ]

instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" $ \o ->
    MkResponseError
      <$> (errorCodeFromInt <$> o .: "code")
      <*> o .: "message"
      <*> o .:? "data"

-- | JSON-RPC 2.0 response (success or error).
data Response = MkResponse
  { resId     :: !Id
  , resResult :: !(Either ResponseError Value)
  } deriving (Show, Eq)

instance ToJSON Response where
  toJSON r = case resResult r of
    Right val -> object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= resId r
      , "result"  .= val
      ]
    Left err -> object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= resId r
      , "error"   .= err
      ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    checkVersion o
    rid <- o .: "id"
    result <- o .:? "result"
    err    <- o .:? "error"
    case (result, err) of
      (Just val, Nothing) -> pure $ MkResponse rid (Right val)
      (Nothing, Just e)   -> pure $ MkResponse rid (Left e)
      _                   -> fail "Response must have exactly one of 'result' or 'error'"

-- | A parsed JSON-RPC 2.0 message — request, notification, or response.
data Message
  = MsgRequest !Request
  | MsgNotification !Notification
  | MsgResponse !Response
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (MsgRequest r)      = toJSON r
  toJSON (MsgNotification n) = toJSON n
  toJSON (MsgResponse r)     = toJSON r

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    checkVersion o
    hasId     <- (True <$ (o .: "id" :: Parser Id)) <|> pure False
    hasMethod <- (True <$ (o .: "method" :: Parser Text)) <|> pure False
    case (hasId, hasMethod) of
      (True, True)   -> MsgRequest <$> parseJSON (Object o)
      (False, True)  -> MsgNotification <$> parseJSON (Object o)
      (True, False)  -> MsgResponse <$> parseJSON (Object o)
      (False, False) -> fail "Invalid JSON-RPC message: missing both 'id' and 'method'"

checkVersion :: Object -> Parser ()
checkVersion o = do
  v <- o .: "jsonrpc" :: Parser Text
  if v == "2.0"
    then pure ()
    else fail $ "Unsupported JSON-RPC version: " ++ show v
