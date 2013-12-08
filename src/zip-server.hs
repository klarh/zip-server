{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Codec.Archive.Zip
import Control.Applicative((<$>))
import Data.Monoid (mconcat)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath.Posix(joinPath)
import System.IO (FilePath(..))

data Options = Options {port :: Int,
                        filename :: String} deriving (Data, Typeable, Show)

defopts = Options {port = 7784,
                   filename = def &= typFile}

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

makeResponse entry = responseLBS status200 [("Content-Type", typ)] (fromEntry entry)
  where
    typ = defaultMimeLookup . T.pack . eRelativePath $ entry

main = do
  (Options port filename) <- cmdArgs defopts

  fileExists <- doesFileExist filename
  archive <- if (fileExists)
    then toArchive <$> B.readFile filename
    else error "Error: zip file not found; specify with --filename"

  let files = filesInArchive archive
  run port $ app files archive
