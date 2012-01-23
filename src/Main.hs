{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Digest.Pure.MD5
import Data.Map (toList)
import Data.String
import Data.Serialize.Get
import Data.Word
import qualified Data.Text as T
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveDirectory)
import Snap.Util.FileUploads
import Text.JSON
import Web.Authenticate.OpenId
import Web.Authenticate.OpenId.Providers

import Graphics.CR2
import Network.Rackspace.CloudAPI
import Util

staticFiles :: Snap ()
staticFiles = do
  let index = path (fromString "") $ sendFile "resources/html/index.html"
  let resources = serveDirectory "resources"
  (loginProtect (\id -> index)) <|> resources

-- Helper const values.
authIO :: IO (Maybe Authorization)
authIO = getAuthorization ("davidhinkes", "5fac51fe3cc8d642db525aedf34c5134")

container :: Container
container = "memento"

archive :: String -> FilePath -> IO ()
archive id file_path = do
  file_contents <- B.readFile file_path
  let (Right attrs) = runGetLazy getCR2Attributes file_contents
  let md5_digest = show $ md5 file_contents
  let cloud_name = id ++ "/" ++ md5_digest
  auth_token <- authIO
  case auth_token of
    Nothing -> print "Authorization failed."
    Just token -> do
                    createContainer token container
                    createFile token container cloud_name file_contents attrs
                    return ()

status :: String -> Snap ()
status who = do
  files <- liftIO status'
  modifyResponse $ setContentType (fromString "test/json")
  writeText $ fromString $ encode $ showJSON files
  return () where
    status' = do
      a <- authIO
      case a of
        Nothing -> return []
        Just auth -> getFileList auth container who

maybeIdentifier :: [(T.Text,T.Text)] -> IO (Maybe String)
maybeIdentifier p =
  Control.Exception.catch getId ex
  where ex (_ :: AuthenticateException) = return Nothing
        getId = do
          (id, _) <- authenticate p
          let hash = md5 $ fromString $ show id
          return $ Just $ show hash

loginProtect :: (String -> Snap ()) -> Snap ()
loginProtect action = do
  -- See if there is a cookie.
  cookie <- getCookie (fromString cookie_id_name)
  case cookie of
    Just c -> action $ bs2s $ cookieValue c
    Nothing -> do
      -- See if there is an openID params in the URL.
      url <- getsRequest extractURL
      params <- getsRequest rqParams
      let params' = toList params
      let params'' = map (\(k,v) -> (fromString $ bs2s k, fromString . bs2s . head $ v)) params' :: [(T.Text,T.Text)]
      id <- liftIO $ maybeIdentifier params''
      case id of
        Just id' -> do
          modifyResponse $ addResponseCookie $ Cookie (fromString cookie_id_name)
                                                      (fromString id')
                                                      Nothing Nothing Nothing False False
          action id'
        Nothing -> do
          -- Redirect to login service.
          r <- liftIO $ getForwardUrl (fromString google) (fromString url) Nothing []
          redirect $ fromString $ T.unpack r
  where extractURL r = "http://" ++ (bs2s $ rqServerName r) ++ ":" ++ (show . rqServerPort $ r) ++ (bs2s . rqURI $ r)
        cookie_id_name = "id"

upload :: String -> Snap ()
upload id = do
  handleFileUploads
    "."
    (setMaximumFormInputSize size defaultUploadPolicy)
    (\_ -> allowWithMaximumSize size)
    (liftIO . msum . (map f'))
  where f' (part_info, Right path) = archive id path
        f' _ = return ()
        size = 50*1024*1024

main :: IO ()
main = do
  quickHttpServe $ staticFiles <|> route [
    (fromString "upload", loginProtect (\r -> upload r)),
    (fromString "status", loginProtect (\r -> status r))
    ]
