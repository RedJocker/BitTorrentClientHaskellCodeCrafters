{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Util (word8ToHex, chuncked) where

import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
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
