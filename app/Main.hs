{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}    
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Tuple (swap)
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Parser

doDecode :: String -> IO ()
doDecode encodedValue = do
  let decodedValue = Parser.decodeBencodedValue(B.pack encodedValue)                   
  let jsonValue = encode decodedValue
  LB.putStr jsonValue
  putStr "\n"

doInfo :: String -> IO ()
doInfo filePath = do
  
  handle <- openBinaryFile filePath ReadMode
  fileContent <- B.hGetContents handle
  let decodedValue = Parser.decodeBencodedValue fileContent
 
  case decodedValue of
    BencDict dict -> do
      let trackerUrl = (Map.!) dict "announce"
      let infoValue = (Map.!) dict "info"
      case (trackerUrl, infoValue) of
        (BencString str, BencDict infoDict) -> do
          let lengthValue = (Map.!) infoDict "length"
          case lengthValue of
            (BencInteger len) -> do
              putStrLn $ "Tracker URL: " ++ (B.unpack str)
              putStrLn $ "Length: " ++ (show len)
            _ -> do return ()
        _ -> do return ()
    _ -> do return ()

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
        "decode" -> doDecode (args !! 1)
        "info" -> doInfo (args !! 1) 
        _ -> putStrLn $ "Unknown command: " ++ command
