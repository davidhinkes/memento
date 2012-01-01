module Main where
import Control.Applicative
import qualified Data.ByteString as B
import Data.String
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveDirectory, serveFile)

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

main :: IO ()
main = do
  quickHttpServe $ (static_files <|> app)

