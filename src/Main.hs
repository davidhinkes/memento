module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Digest.Pure.MD5
import Data.String
import Data.Serialize.Get
import Data.Word
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveDirectory)
import Snap.Util.FileUploads

import Graphics.CR2
import Network.Rackspace.CloudAPI

staticFiles :: Snap ()
staticFiles = do
  let index = path (fromString "") $ sendFile "resources/html/index.html"
  let resources = serveDirectory "resources"
  index <|> resources

app :: Snap ()
app = do
  return ()

archive :: FilePath -> IO ()
archive file_path = do
  file_contents <- B.readFile file_path
  let (Right attrs) = runGetLazy getCR2Attributes file_contents
  let md5_digest = show $ md5 file_contents
  auth_token <- getAuthorization ("davidhinkes", "5fac51fe3cc8d642db525aedf34c5134")
  case auth_token of
    Nothing -> print "Authorization failed."
    Just token -> do
                    let container = "testcontainer2"
                    createContainer token container
                    createFile token container md5_digest file_contents attrs
                    return ()

upload :: Snap ()
upload = handleFileUploads
  "."
  (setMaximumFormInputSize size defaultUploadPolicy)
  (\_ -> allowWithMaximumSize size)
  (liftIO . msum . (map f'))
  where f' (part_info, Right path) = archive path
        f' _ = return ()
        size = 50*1024*1024

main :: IO ()
main = do
  quickHttpServe $ staticFiles <|> path (fromString "upload") upload <|> app

