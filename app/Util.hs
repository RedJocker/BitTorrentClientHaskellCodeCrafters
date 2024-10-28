{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Util (
  word8ToHex
  , chuncked
  , chunckedLst
  , ipStr
  , toHex
  , word64ToBytes
  , byteStringToInt
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8, Word64)
import Data.List (intercalate)
import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Numeric (showHex)

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

byteStringToInt :: ByteString -> Int
byteStringToInt = BS.foldl
      (\acc cur -> 256 * acc + fromIntegral cur)
      0

word64ToBytes :: Word64 -> [Word8]
word64ToBytes num =
  let helper cur acc pos =
        let (d, m) = cur `divMod` 256
        in
          if pos == 4 then
            fromIntegral m : acc
          else
            helper d (fromIntegral m : acc) (pos + 1)
  in helper num [] 1

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
