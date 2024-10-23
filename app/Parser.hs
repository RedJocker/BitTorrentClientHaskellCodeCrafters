{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
  Bencoded(BencString, BencInteger, BencList, BencDict),
  decodeBencodedValue,
  bencodeValue
) where

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit, ord)
import Data.Tuple (swap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map

a |> f = f a

data Bencoded =
  BencString !ByteString
  | BencInteger !Int
  | BencList ![Bencoded]
  | BencDict !(Map.Map String Bencoded)
  deriving (Show, Ord, Eq)

instance ToJSON Bencoded where
  toJSON (BencString string) = toJSON (B.unpack string)
  toJSON (BencInteger integer) = toJSON integer
  toJSON (BencList lst) = toJSON lst
  toJSON (BencDict dict) = toJSON dict

parseBencodedString :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedString encodedValue =
  B.readInt encodedValue
      >>= (\(len, cRest) ->
             B.stripPrefix ":" cRest   -- drop ':'
             >>= (\rest -> rest
                  |> B.splitAt len       -- (numStr, rest)
                  |> swap                -- (rest, numStr)
                  |> (BencString <$>)      -- (rest, bencStr)
                  |> Just))

parseBencodedInteger :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedInteger encodedValue =
  (B.stripPrefix "i" encodedValue >>= B.readInt)
      >>= (\(value, eRest) ->
             B.stripPrefix "e" eRest
             >>= (\rest -> return (rest, BencInteger value)))

parseBencodedList :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedList encodedValue =
  let parseLst acc str
        | (==) "" str = Nothing
        | (==) 'e' (B.head str) =
            return (B.tail str, BencList (reverse acc))
        | otherwise = parseBencodedValue str
                      >>= \(rest, value) -> parseLst (value:acc) rest
  in
    B.stripPrefix "l" encodedValue
    >>= parseLst []

parseBencodedDict :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedDict encodedValue =
  let parseKeyValue str =
        parseBencodedValue str
        >>= (\(rest, BencString key) ->
                parseBencodedValue rest
                >>= (\(rest', value) ->
                        return (rest', B.unpack key, value)))

      parseDict acc str
        | (==) "" str = Nothing
        | (==) 'e' (B.head str) = Just (B.tail str, reverse acc)
        | otherwise = parseKeyValue str
                      >>= \(rest, key, value) ->
                            parseDict ((key, value):acc) rest

  in
    B.stripPrefix "d" encodedValue
    >>= parseDict []
    >>= (\(rest, assocLst) -> Just (rest, BencDict (Map.fromList assocLst)))

parseBencodedValue :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedValue encodedValue
  | isDigit (B.head encodedValue) =
      parseBencodedString encodedValue
  | (==) 'i' (B.head encodedValue) =
      parseBencodedInteger encodedValue
  | (==) 'l' (B.head encodedValue) =
      parseBencodedList encodedValue
  | (==) 'd' (B.head encodedValue) =
      parseBencodedDict encodedValue
  | otherwise = Nothing

decodeBencodedValue :: ByteString -> Bencoded
decodeBencodedValue encodedValue =
  case parseBencodedValue encodedValue of
    Just ("", decoded) -> decoded
    _otherwise -> error $ "Unhandled encoded value: " ++ B.unpack encodedValue

bencodeValue :: Bencoded -> ByteString
bencodeValue (BencString str) =
  let lenStr = (B.pack . show . B.length) str
  in
    lenStr
    |> \s -> BS.append s (B.pack ":")
    |> \s -> BS.append s str

bencodeValue (BencInteger int) =
  let intStr = (B.pack . show) int
  in
    intStr
    |> BS.cons ((fromIntegral . ord) 'i')
    |> \s -> BS.append s "e"

bencodeValue (BencList lst) =
   BS.concat (bencodeValue <$> lst)
   |> BS.cons ((fromIntegral . ord) 'l')
   |> \s -> BS.append s "e"

bencodeValue (BencDict dict) =
   let assoc = Map.toAscList dict
       entryAsBencStr (key, value) =
         let keyLen = (show . length) key
             keyBenc = B.pack (keyLen ++ ":" ++ key)
             valueBenc = bencodeValue value
         in
           B.append keyBenc valueBenc
   in
   BS.concat (entryAsBencStr <$> assoc)
   |> BS.cons ((fromIntegral . ord) 'd')
   |> \s -> BS.append s "e"
