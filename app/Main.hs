{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
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
import Crypto.Hash.SHA1 as SHA1
import Util
import Network.Simple.TCP (connect, HostName, ServiceName)
import Network.Socket.ByteString (sendAll, send)
import Network.HTTP.Base (urlEncodeVars)
import Text.Printf (printf)
import Text.Read (readMaybe)

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
  ,  infoHash :: !ByteString
  , infoHashQueryParam :: !String
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
      infoHash = SHA1.hash infoBencStr
      infoHashHex = toHex infoHash
      infoHashUrlEnc =  B.concat (B.cons '%' <$>  chuncked 2 (B.pack infoHashHex))
  in
    TrackerRequest
    { trackerUrl = B.unpack host
    , infoHash = infoHash
    , infoHashQueryParam = B.unpack infoHashUrlEnc
    , hostname = B.unpack hostname
    , hostport = if "" /= hostport then
                    B.unpack (B.dropWhile (== ':') hostport)
                 else "80"
    , peerId = "09876543210123456789"
    , port = 6881
    , uploaded = 0
    , downloaded = 0
    , left = pieceLenght
    , compact = 1
    }


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
              let sha1 = SHA1.hash infoBencoded
              putStrLn $ "Info Hash: " <> toHex sha1
              putStrLn $ "Piece Length: " ++ show piecesLen
              let piecesHash = chuncked 40 ((B.pack . toHex) pieces)
              putStrLn "Piece Hashes:"
              mapM_ B.putStrLn piecesHash
            _otherwise -> return ()
        _otherwise -> return ()
    _otherwise -> return ()

getPeerIps :: TrackerRequest -> IO [String]
getPeerIps trackerRequest = do
  let path = "/announce?info_hash="
             <> infoHashQueryParam trackerRequest
             <> "&"
             <> trackerRequestQueryString trackerRequest

  connect (hostname trackerRequest) (hostport trackerRequest) $ \(socket, remoteAddr) -> do
    let requestStr = intercalate "\r\n"
                     [ "GET " ++ path ++ " HTTP/1.1"
                     , "Host: " ++ hostname trackerRequest
                     ] ++ "\r\n\r\n"

    send socket (B.pack requestStr)
    (head, body) <- recvHttp socket
    let (BencDict bodyBenc) = decodeBencodedValue body
    let (BencString peersBytes) = (Map.!) bodyBenc "peers"
    let byteArr = BS.unpack peersBytes
    return $ ipStr <$> chunckedLst 6 byteArr

doPeers :: String -> IO ()
doPeers filePath = do
  torrentBenc <- getBencFromTorrentFile filePath
  let request = trackerRequest torrentBenc
  peerIps <- getPeerIps request
  mapM_ putStrLn peerIps

connectPeer :: TrackerRequest
  -> HostName -> ServiceName -> (String ->IO r) -> IO r
connectPeer trackerRequest peerIp peerPort onConnect = do
  let lenProtocolString = BS.pack [19]
  let protocolString = B.pack "BitTorrent protocol"
  let reservedBytes = BS.pack (replicate 8 0)

  let TrackerRequest {infoHash=infoHash, peerId=peerId} = trackerRequest
  let handshakeMsg = lenProtocolString
        <> protocolString
        <> reservedBytes
        <> infoHash
        <> B.pack peerId
  let port = if peerPort == "" then "80" else peerPort

  connect peerIp port $ \(socket, remoteAddr) -> do

    send socket handshakeMsg
    response <- recvPeerHandshake socket
    let peerId = concatMap word8ToHex ((BS.unpack . B.drop 48) response)
    --putStrLn $ "Response: " <> (B.unpack response)
    onConnect peerId

doHandshake :: String -> String -> String  -> IO ()
doHandshake torrentFile peerIp peerPort = do
  torrentBenc <- getBencFromTorrentFile torrentFile
  let request = trackerRequest torrentBenc
  connectPeer request peerIp peerPort
    (\peerId -> putStrLn $ "Peer ID: " <> peerId)


doDownloadPiece :: String -> String -> Int  -> IO ()
doDownloadPiece outFile torrentFile pieceIndex = do

  torrentBenc <- getBencFromTorrentFile torrentFile
  let request = trackerRequest torrentBenc
  peerIps <- getPeerIps request
  let targetIp = peerIps !! 0
  let (peerHostname, peerPort) = dropWhile (== ':') <$> break (== ':') targetIp
  printf "outFile: %s torrentFile: \
         \%s pieceIndex %d\n" outFile torrentFile pieceIndex
  connectPeer request peerHostname peerPort
    (\peerId -> putStrLn $ "Peer ID: " <> peerId)


main :: IO ()
main = do
    args <- getArgs
    when (length (take 2 args) < 2) $ do
            putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)

    let command = head args
    let arg = args !! 1
    case command of
        "decode" -> doDecode arg
        "info" -> doInfo arg
        "peers" -> doPeers arg
        "handshake"-> do
            when (length (take 3 args) < 3) $ do
                putStrLn "Usage: your_bittorrent.sh \
                         \handshake <file> <peer_ip>:<peer_port>"
                exitWith (ExitFailure 1)
            let (peerIp, peerPort) =
                  dropWhile (== ':' ) <$> break (== ':') (args !! 2)
            doHandshake arg peerIp peerPort
        "download_piece" -> do
            when (length (take 5 args) < 5) $ do
                putStrLn "Usage: your_bittorrent.sh \
                         \download_piece -o \
                         \<out_file> <torrent_file> <piece_index>"
                exitWith (ExitFailure 1)
            let pieceIndex = fromMaybe (-1 :: Int) (readMaybe (args !! 4))
            if pieceIndex < 0 then
              putStrLn "piece_index must be a positive number"
            else
              doDownloadPiece (args !! 2) (args !! 3) pieceIndex
        _ -> putStrLn $ "Unknown command: " ++ command
