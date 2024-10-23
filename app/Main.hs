{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Word (Word8)
import Data.List (intercalate)
import System.Environment
import System.Exit
import System.IO
import GHC.Generics (Generic)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Parser
import Crypto.Hash as Hash
import Numeric (showHex)
import Util
import Network.Simple.TCP (connect)
import Network.Socket.ByteString (sendAll, send)
import Network.HTTP.Base (urlEncodeVars)
-- use the following command to create a language server for this ghc version
-- ghcup compile hls --version 2.6.0.0 --ghc 9.4.6

doDecode :: String -> IO ()
doDecode encodedValue = do
  let decodedValue = Parser.decodeBencodedValue (B.pack encodedValue)
  let jsonValue = encode decodedValue
  LB.putStr jsonValue
  putStr "\n"

getBencFromTorrentFile :: String -> IO Bencoded
getBencFromTorrentFile filePath = do
  handle <- openBinaryFile filePath ReadMode
  fileContent <- B.hGetContents handle
  return (Parser.decodeBencodedValue fileContent)

data TrackerRequest = TrackerRequest
  { trackerUrl :: !String
  ,  hostname :: !String
  ,  hostport :: !String
  , infoHash :: !String
  , peerId :: !String
  , port  :: !Int
  , uploaded  :: !Int
  , downloaded  :: !Int
  , left  :: !Int
  , compact  :: !Int
  } deriving (Show, Generic)

trackerRequest :: Bencoded -> TrackerRequest
trackerRequest (BencDict torrentDict) =
  let (BencString url) = (Map.!) torrentDict "announce"
      path = fromMaybe url (B.stripPrefix "http://" url)
      host = fromMaybe path (B.stripSuffix "/announce" path)
      (hostname, hostport) = B.break (== ':') host
      infoBenc@(BencDict infoDict) = (Map.!) torrentDict "info"
      (BencInteger infoLength) = (Map.!) infoDict "length"
      (BencInteger pieceLenght) = (Map.!) infoDict "piece length"
      infoBencStr = bencodeValue infoBenc
      sha1 = show (Hash.hash infoBencStr :: Hash.Digest Hash.SHA1)
      sha1UrlEnc =  B.concat $ B.cons '%' <$>  chuncked 2 (B.pack sha1)
  in
    TrackerRequest
    { trackerUrl = B.unpack host
    , infoHash = B.unpack sha1UrlEnc
    , hostname = B.unpack hostname
    , hostport = if ("" /= hostport) then
                    B.unpack (B.dropWhile (== ':') hostport)
                 else "80"
    , peerId = "09876543210123456789"
    , port = 6881
    , uploaded = 0
    , downloaded = 0
    , left = pieceLenght
    , compact = 1
    }


-- Function to convert TorrentParams to query string
trackerRequestQueryString :: TrackerRequest -> String
trackerRequestQueryString params =
    urlEncodeVars
        [ ("peer_id", peerId params)
        , ("port", show $ port params)
        , ("uploaded", show $ uploaded params)
        , ("downloaded", show $ downloaded params)
        , ("left", show $ left params)
        , ("compact", show $ compact params)
        ]


doInfo :: String -> IO ()
doInfo filePath = do

  decodedValue <- getBencFromTorrentFile filePath

  case decodedValue of
    BencDict dict -> do
      let trackerUrlValue = (Map.!) dict "announce"
      let infoValue = (Map.!) dict "info"
      case (trackerUrlValue, infoValue) of
        (BencString trackerUrl, BencDict infoDict) -> do
          let infoLengthValue = (Map.!) infoDict "length"
          let piecesLenghtValue = (Map.!) infoDict "piece length"
          let piecesValue = (Map.!) infoDict "pieces"
          let infoBencoded = bencodeValue infoValue
          case (infoLengthValue, piecesLenghtValue, piecesValue) of
            (BencInteger infoLen, BencInteger piecesLen, BencString pieces) -> do
              putStrLn $ "Tracker URL: " ++ B.unpack trackerUrl
              putStrLn $ "Length: " ++ show infoLen
              let sha1 = (Hash.hash infoBencoded :: Hash.Digest Hash.SHA1)
              putStr "Info Hash: "
              print sha1
              putStrLn $ "Piece Length: " ++ show piecesLen
              let piecesHash =
                   chuncked 40 (B.concat $ B.pack . word8ToHex <$> BS.unpack pieces)
              putStrLn "Piece Hashes:"
              mapM_ B.putStrLn piecesHash
            _otherwise -> return ()
        _otherwise -> return ()
    _otherwise -> return ()

doPeers :: String -> IO ()
doPeers filePath = do
  decodedValue <- getBencFromTorrentFile filePath
  let request = trackerRequest decodedValue
  let path = "/announce?info_hash=" <> infoHash request <> "&" <> trackerRequestQueryString request
  --putStrLn path
  connect (hostname request) (hostport request) $ \(socket, remoteAddr) -> do
    --putStrLn (show remoteAddr)
    let requestStr = intercalate "\r\n"
                [ "GET " ++ path ++ " HTTP/1.1"
                , "Host: " ++ hostname request
                ] ++ "\r\n\r\n"
    --putStrLn requestStr
    send socket (B.pack requestStr)
    response <- recvAll socket
    --putStrLn (B.unpack response)
    
    let responseLst = B.dropWhile (== '\n')  <$> B.split '\r' response
    let (headLst, bodyLst) = break (== "") responseLst
    let (BencDict bodyBenc) = decodeBencodedValue ( B.concat bodyLst )
    
    let (BencString peersBytes) = (Map.!) bodyBenc "peers"
    let byteArr = BS.unpack peersBytes
    let ips = ipStr <$> chunckedLst 6 byteArr
     
    mapM_ putStrLn ips 
    --mapM_ B.putStrLn body
    --putStrLn (B.unpack response)


main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
            putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)

    let command = head args
    let arg = args !! 1
    case command of
        "decode" -> doDecode arg
        "info" -> doInfo arg
        "peers" -> doPeers arg
        _ -> putStrLn $ "Unknown command: " ++ command
