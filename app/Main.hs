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


data Bencoded =
  BencString ByteString
  | BencInteger Int
  | BencList [Bencoded]
  deriving (Show)

instance ToJSON Bencoded where
  toJSON (BencString string) = toJSON (B.unpack string)
  toJSON (BencInteger integer) = toJSON integer
  toJSON (BencList lst) = toJSON (toJSON <$> lst)

a |> f = f a

parseBencodedValue :: ByteString -> Maybe (ByteString, Bencoded)
parseBencodedValue encodedValue
  | isDigit (B.head encodedValue) =
      (B.readInt encodedValue)
      >>= (\(len, cRest) ->
             (B.stripPrefix ":" cRest)   -- drop ':'
             >>= (\rest -> rest
                  |> (B.splitAt len)       -- (numStr, rest)
                  |> (swap)                -- (rest, numStr)
                  |> (BencString <$>)      -- (rest, bencStr)
                  |> Just))
  | (==) 'i' (B.head encodedValue) =
      ((B.stripPrefix "i" encodedValue) >>= B.readInt)
      >>= (\(value, eRest) ->
             (B.stripPrefix "e" eRest)
             >>= (\rest -> Just (rest, BencInteger value)))
  | (==) 'l' (B.head encodedValue) =
      let parseLst acc str
            | (==) "" str = Nothing
            | (==) 'e' (B.head str) =
              Just (B.tail str, (BencList (reverse acc)))
            | otherwise = parseBencodedValue str
              >>= \(rest, value) -> parseLst (value:acc) rest
      in
      (B.stripPrefix "l" encodedValue)
      >>= (parseLst [])
  | otherwise = error $ "Unhandled encoded value: " ++ B.unpack encodedValue

-- decodeBencodedValue :: ByteString -> Bencoded
-- decodeBencodedValue encodedValue
--     | isDigit (B.head encodedValue) =
--         case B.readInt encodedValue of
--             Just (len, rest) -> (BencString . B.drop (1)) rest
--             Nothing -> error "Invalid encoded value"
--     | (==) 'i' (B.head encodedValue) =
--         case (B.stripPrefix "i" encodedValue) >>= B.readInt of
--           Just (value, "e") -> BencInteger value
--           _ -> error "Invalid encoded value"
--     | (==) 'l' (B.head encodedValue) =
--       case (B.stripPrefix "l" encodedValue) >>= (B.stripSuffix "e") of
--           Just elements -> BencList [ BencString "" ]
--           _ -> error "Invalid encoded value"
--     | otherwise = error $ "Unhandled encoded value: " ++ B.unpack encodedValue

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
