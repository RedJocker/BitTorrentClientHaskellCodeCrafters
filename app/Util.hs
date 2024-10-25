{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Util (
  word8ToHex
  , chuncked
  , chunckedLst
  , recvAll
  , ipStr
  , toHex
  , recvPeerHandshake
  , recvHttp
  ) where
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
import Data.List (intercalate)
import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Numeric (showHex)
import Network.Simple.TCP

word8ToHex :: Word8 -> String
word8ToHex w =
  let
    h = showHex w ""
  in
    if length h == 1 then '0' : h else h

chuncked :: Int -> ByteString -> [ByteString]
chuncked size str =
  let helper acc cur =
        case cur of
          "" -> reverse acc
          _ ->  helper ((B.take size cur):acc) (B.drop size cur) 
  in helper [] str 

chunckedLst :: Eq a => Int -> [a] -> [[a]]
chunckedLst size lst =
  let helper acc cur
        | mempty == cur = reverse acc 
        | otherwise = helper ((take size cur):acc) (drop size cur) 
  in helper mempty lst 

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


ipStr :: [Word8] -> String 
ipStr [a,b,c,d,e,f] = 
  let port = (shiftL (fromEnum e) 8) + (fromEnum f)
      nibbles = show . fromIntegral <$> [a,b,c,d]
      ipHostStr = intercalate "." nibbles 
      portStr = show  port 
  in
    ipHostStr <> ":" <> portStr 

toHex :: ByteString -> String
toHex str = concatMap word8ToHex (BS.unpack str)
