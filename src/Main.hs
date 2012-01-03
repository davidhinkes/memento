module Main where
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String
import Data.Digest.Pure.MD5
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveDirectory, serveFile)
import Snap.Util.FileUploads

import Network.Rackspace.CloudAPI

static_files :: Snap ()
static_files = do
  let index = path (fromString "") $ sendFile "resources/html/index.html"
  let resources = serveDirectory "resources"
  index <|> resources

app :: Snap ()
app = do
  msg_param <- getParam $ fromString "msg"
  case msg_param of
    Just value -> writeBS $ B.concat [ fromString "You said: ", value ]
    _ -> writeLBS $ fromString "You said nothing"

archive :: FilePath -> IO ()
archive file_path = do
  file_contents <- LB.readFile file_path
  let md5_digest = show $ md5 file_contents
  putStrLn md5_digest

upload :: Snap ()
upload = handleFileUploads
  "."
  (setMaximumFormInputSize size defaultUploadPolicy)
  (\_ -> allowWithMaximumSize size)
  (\fs -> liftIO $ msum $ map f' fs)
  where f' (part_info, Right path) = archive path
        f' _ = return ()
        size = 50*1024*1024

main :: IO ()
main = do
  auth_token <- getAuthToken ("davidhinkes", "5fac51fe3cc8d642db525aedf34c5134")
  case auth_token of
    Nothing -> return ()
    Just token -> putStrLn . show $ token
  quickHttpServe $ (static_files <|> (path (fromString "upload") upload) <|> app)
