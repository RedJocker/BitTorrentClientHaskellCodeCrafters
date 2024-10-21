{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}    
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Tuple (swap)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map

a |> f = f a

data Bencoded =
  BencString ByteString
  | BencInteger Int
  | BencList [Bencoded]
  | BencDict (Map.Map String Bencoded)
  deriving (Show, Ord, Eq)

instance ToJSON Bencoded where
  toJSON (BencString string) = toJSON (B.unpack string)
  toJSON (BencInteger integer) = toJSON integer
  toJSON (BencList lst) = toJSON lst
  toJSON (BencDict dict) = toJSON dict

parseBencodedString :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedString encodedValue =
  (B.readInt encodedValue)
      >>= (\(len, cRest) ->
             (B.stripPrefix ":" cRest)   -- drop ':'
             >>= (\rest -> rest
                  |> (B.splitAt len)       -- (numStr, rest)
                  |> (swap)                -- (rest, numStr)
                  |> (BencString <$>)      -- (rest, bencStr)
                  |> Just))

parseBencodedInteger :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedInteger encodedValue =
  ((B.stripPrefix "i" encodedValue) >>= B.readInt)
      >>= (\(value, eRest) ->
             (B.stripPrefix "e" eRest)
             >>= (\rest -> Just (rest, BencInteger value)))

parseBencodedList :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedList encodedValue =
  let parseLst acc str
        | (==) "" str = Nothing
        | (==) 'e' (B.head str) =
            Just (B.tail str, (BencList (reverse acc)))
        | otherwise = parseBencodedValue str
                      >>= \(rest, value) -> parseLst (value:acc) rest
  in
    (B.stripPrefix "l" encodedValue)
    >>= (parseLst [])

parseBencodedDict :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedDict encodedValue =
  let parseKeyValue str =
        parseBencodedValue str
        >>= (\(rest, BencString key) ->
                parseBencodedValue rest
                >>= (\(rest', value) ->
                        Just (rest', B.unpack key, value)))

      parseDict acc str
        | (==) "" str = Nothing
        | (==) 'e' (B.head str) = Just (B.tail str, reverse acc)
        | otherwise = parseKeyValue str
                      >>= \(rest, key, value) -> parseDict ((key, value):acc) rest
                                                 
  in
    (B.stripPrefix "d" encodedValue)
    >>= (parseDict [])
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
    otherwise -> error $ "Unhandled encoded value: " ++ B.unpack encodedValue 

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then do 
            putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)
        else return ()

    let command = args !! 0
    case command of
        "decode" -> do
          -- You can use print statements as follows for debugging, they'll be visible when running tests.           
            let encodedValue = args !! 1
            let decodedValue = decodeBencodedValue(B.pack encodedValue)
            
            let jsonValue = encode decodedValue
            LB.putStr jsonValue
            putStr "\n"
        _ -> putStrLn $ "Unknown command: " ++ command
