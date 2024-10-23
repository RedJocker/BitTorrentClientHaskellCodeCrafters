{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Util (
  word8ToHex
  , chuncked
  , chunckedLst
  , recvAll
  , ipStr
  ) where
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
import Data.List (intercalate)
import Data.Bits (shiftL)
import qualified Data.ByteString.Char8 as B
import Numeric (showHex)
import Network.Simple.TCP
import Data.Maybe (fromJust)
import Control.Monad.IO.Class 
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

ipStr :: [Word8] -> String 
ipStr [a,b,c,d,e,f] = 
  let port = (shiftL (fromEnum e) 8) + (fromEnum f)
      nibbles = show . fromIntegral <$> [a,b,c,d]
      ipHostStr = intercalate "." nibbles 
      portStr = show  port 
  in
    ipHostStr <> ":" <> portStr 




