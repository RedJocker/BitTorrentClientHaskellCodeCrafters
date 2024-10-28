{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Networking (
   recvAll
  , recvPeerHandshake
  , recvPeerMessage
  , recvHttp
  , recvInt
  , recvId 
  , recvBody
  , sendInterested
  , sendRequest
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Network.Simple.TCP
import Util (byteStringToInt, word64ToBytes)

recvAll :: Socket -> IO ByteString 
recvAll socket = do
  go mempty
  where
    go acc = do
      chunk <- recv socket 4096
      case chunk of
        Just chk -> do
          let len = B.length chk
          if len < 4096 then
            pure (acc <> chk)
          else 
            go (acc <> chk)
        Nothing -> do
          pure acc

recvPeerMessage :: Socket -> IO (Int, ByteString, ByteString) 
recvPeerMessage socket = do
  messageLen <- recvInt socket
  messageId <- recvId socket
  body <- recvBody socket (messageLen - 1)
  return (messageLen, messageId, body)

recvInt :: Socket -> IO Int
recvInt socket = do
  response <- go mempty
  pure $ byteStringToInt response 
  where
    go acc = do
      maybeChunk <- recv socket 4
      case maybeChunk of
        Just chunk -> do
          let len = B.length chunk
          if len < 4 then
            go (acc <> chunk)
          else 
            pure (acc <> chunk)
        Nothing -> do
          pure acc

recvId :: Socket -> IO ByteString  
recvId socket = do
  go mempty
  where
    go acc = do
      maybeChunk <- recv socket 1
      case maybeChunk of
        Just chunk -> do
          let len = B.length chunk
          if len < 1 then
            go (acc <> chunk)
          else 
            pure (acc <> chunk)
        Nothing -> do
          pure acc


recvBody :: Socket -> Int -> IO ByteString  
recvBody socket bodyLen
  | bodyLen > 0 =
      let go acc len = do
            maybeChunk <- recv socket (min bodyLen 4096)
            case maybeChunk of
              Just chunk -> do
                let currBdy = acc <> chunk 
                let chunkLen = B.length currBdy
                if chunkLen < bodyLen then
                  go currBdy (len - chunkLen)
                else
                  pure (acc <> chunk)
              Nothing -> do
                pure acc
      in
        go mempty bodyLen
  | otherwise = return mempty

recvHttp :: Socket -> IO (ByteString , ByteString)
recvHttp socket = do
  response <- recvAll socket
  return $ BS.drop 4 <$> BS.breakSubstring (B.pack "\r\n\r\n") response


recvPeerHandshake :: Socket -> IO ByteString 
recvPeerHandshake socket = do
  go mempty
  where
    go acc = do
      chunk <- recv socket 68
      case chunk of
        Just chk -> do
            pure (acc <> chk)
        Nothing -> do
          pure acc

sendMessage socket len id body = do
  send socket (BS.pack (word64ToBytes len ++ [id] ++ body)) 

sendInterested socket = do
  let len = 1
  let id = 2
  let body = []
  sendMessage socket len id body

sendRequest socket pieceIndex blockOffset blockLen = do
  let len = 13
  let id = 6
  let body = word64ToBytes (fromIntegral pieceIndex) ++    
             word64ToBytes blockOffset ++                  
             word64ToBytes blockLen
  sendMessage socket len id body
