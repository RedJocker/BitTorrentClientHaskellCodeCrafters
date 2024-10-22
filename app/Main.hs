{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Tuple (swap)
import Data.Word (Word8)
import System.Environment
import System.Exit
import System.IO
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Parser
import Crypto.Hash as Hash
import Numeric (showHex)

-- use the following command to create a language server for this ghc version
-- ghcup compile hls --version 2.6.0.0 --ghc 9.4.6

doDecode :: String -> IO ()
doDecode encodedValue = do
  let decodedValue = Parser.decodeBencodedValue (B.pack encodedValue)
  let jsonValue = encode decodedValue
  LB.putStr jsonValue
  putStr "\n"

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
          let infoLengthValue = (Map.!) infoDict "length"
          let piecesLenghtValue = (Map.!) infoDict "piece length"
          let piecesValue = (Map.!) infoDict "pieces"
          let infoBencoded = bencodeValue infoValue
          case (infoLengthValue, piecesLenghtValue, piecesValue) of
            (BencInteger infoLen, BencInteger piecesLen, BencString pieces) -> do
              putStrLn $ "Tracker URL: " ++ B.unpack str
              putStrLn $ "Length: " ++ show infoLen
              let sha1 = (Hash.hash infoBencoded :: Hash.Digest Hash.SHA1)
              putStr "Info Hash: "
              print sha1
              putStrLn $ "Piece Length: " ++ show piecesLen
              let piecesHash =
                   chuncked 40 (B.concat $ (B.pack . word8ToHex) <$> (BS.unpack pieces))
              putStrLn "Piece Hashes:"
              mapM_ B.putStrLn piecesHash
            _otherwise -> do return ()
        _otherwise -> do return ()
    _otherwise -> do return ()

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
            putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)

    let command = args !! 0
    case command of
        "decode" -> doDecode (args !! 1)
        "info" -> doInfo (args !! 1)
        _ -> putStrLn $ "Unknown command: " ++ command
