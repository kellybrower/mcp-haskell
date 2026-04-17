{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.Stdio
  ( runStdio
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import System.IO (hFlush, hSetBuffering, hIsEOF, stdin, stdout, stderr, BufferMode(..), hPutStrLn)

import MCP.JsonRpc
import MCP.Server

runStdio :: McpServer -> IO ()
runStdio srv = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  loop `catch` handleIOError
  where
    loop = do
      eof <- hIsEOF stdin
      if eof
        then pure ()
        else do
          line <- BS8.hGetLine stdin
          if BS.null line
            then loop
            else do
              processLine srv line
              loop
    handleIOError :: IOException -> IO ()
    handleIOError _ = pure ()

processLine :: McpServer -> BS.ByteString -> IO ()
processLine srv line = case eitherDecodeStrict' line of
  Left err -> do
    hPutStrLn stderr $ "JSON parse error: " ++ err
    let errMsg = MsgResponse $ MkResponse (IdInt 0) $ Left $
          MkResponseError ParseError "Parse error" Nothing
    sendMessage errMsg
  Right msg -> do
    result <- handleMessage srv msg
    case result of
      Just response -> sendMessage response
      Nothing       -> pure ()

sendMessage :: Message -> IO ()
sendMessage msg = do
  LBS.hPut stdout (encode msg)
  BS.hPut stdout "\n"
  hFlush stdout
