module Network.Rackspace.CloudAPI (
  Authorization(Authorization),
  CloudAPI,
  Container,
  MetaData,
  getAuthorization,
  getContainers,
  createContainer,
  createFile,
  getFileList,
) where

import qualified Data.ByteString.Lazy as B
import Network.Curl

type AccountName = String
type AccountAPIKey = String
type CDNURL = String
type StorageURL = String
type AuthToken = String
type Container = String
type MetaData = [(String, String)]
newtype Authorization = Authorization (CDNURL, StorageURL, AuthToken)
  deriving (Show)

class Monad m => CloudAPI m where
  getAuthorization :: (AccountName, AccountAPIKey) -> m (Maybe Authorization)
  getContainers :: Authorization -> m [Container]
  createContainer :: Authorization -> Container -> m Bool
  createFile :: Authorization -> Container -> String -> B.ByteString -> MetaData -> m Bool
  getFileList :: Authorization -> Container -> String -> m [ String ]
