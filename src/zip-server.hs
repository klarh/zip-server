{-# LANGUAGE OverloadedStrings #-}

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
import System.Environment
import System.FilePath.Posix(joinPath)
import System.IO (FilePath(..))

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
  (port:fileName:_) <- getArgs
  archive <- toArchive <$> B.readFile fileName
  let files = filesInArchive archive
      port' = read port :: Int
  run port' $ app files archive
