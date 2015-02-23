{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Codec.Archive.Zip
import Control.Applicative((<$>))
import Data.Maybe
import Data.Monoid (mconcat)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Mime
import Network.Socket (AddrInfoFlag(..), Family(..), PortNumber(..),
                       SockAddr(..), SocketOption(..), SocketType(..), addrAddress, addrFamily,
                       addrFlags, addrProtocol, addrSocketType, bind,
                       defaultHints, defaultProtocol, getAddrInfo, listen,
                       setSocketOption, socket)
import Network.Wai
import Network.Wai.Handler.Warp (HostPreference(..), Settings(..),
                                 defaultSettings, runSettingsSocket, setPort)
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath.Posix(joinPath)
import System.IO (FilePath(..))

data Options = Options {port :: Int,
                        host :: String,
                        filename :: String} deriving (Data, Typeable, Show)

defopts = Options {port = 7784 &= help "Port to listen on" &= typ "PORT",
                   host = "*" &= help "Hostname to use (* for any)" &= typ "HOST",
                   filename = def &= help "Zip file to serve from" &= typFile}
          &= program "zip-server"
          &= summary ""
          &= help "Serve a static website from a zip file"

app contents zipFile req = do
  let path = joinPath $ T.unpack <$> pathInfo req
      response = case findEntryByPath path zipFile of
        Nothing -> listFiles contents
        Just entry -> makeResponse entry
  return response

listFiles contents = responseLBS status200 [("Content-Type", "text/html")] conts
  where
    conts = mconcat [open, middle, close] :: B.ByteString
    open = "<!DOCTYPE html><html><body><ul>" :: B.ByteString
    close = "</ul></body></html>"
    middle = mconcat $ middle' . encodeUtf8 . LT.pack <$> contents
    middle' x = mconcat ["<li><a href=\"", x, "\">", x, "</a></li>"] :: B.ByteString

makeResponse entry = responseLBS status200 headers entry'
  where
    headers = [(hContentType, typ), (hContentLength, clength)] :: ResponseHeaders
    typ = defaultMimeLookup . T.pack . eRelativePath $ entry
    clength = BS.concat . B.toChunks . encodeUtf8 . LT.pack . show . B.length $ entry'
    entry' = fromEntry entry

main = do
  (Options port host filename) <- cmdArgs defopts

  let port' = PortNum . fromIntegral $ port
      aiFlags = [AI_PASSIVE]
      hints = Just $ defaultHints {addrFlags=aiFlags, addrSocketType=Stream}

  fileExists <- doesFileExist filename
  archive <- if (fileExists)
    then toArchive <$> B.readFile filename
    else error "Error: zip file not found; specify with --filename"

  (toBind:_) <- getAddrInfo hints (Just host) (Just . show $ port)

  sock <- socket (addrFamily toBind) (addrSocketType toBind) (addrProtocol toBind)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress toBind)
  listen sock 3

  let files = filesInArchive archive
      settings = setPort port $ defaultSettings
  runSettingsSocket settings sock $ app files archive
