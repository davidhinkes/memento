module Network.Rackspace.CloudAPI
  (
  getAuthToken
  ) where

import Data.Maybe
import Network.Curl
import Network.Curl.Opts

type UserName = String
type UserKey = String
type CDNURL = String
type StorageURL = String
type AuthToken = String
data Authorization = Authorization (CDNURL, StorageURL, AuthToken)
  deriving (Show)

getAuthToken :: (UserName, AuthToken) -> IO (Maybe Authorization)
getAuthToken (user, key) = do
  let headers = ["X-Auth-User: "++user, "X-Auth-Key: "++key]
  resp <- curlGetResponse "https://auth.api.rackspacecloud.com/v1.0" [CurlHttpHeaders headers]
  return $ pullHeaders $ respHeaders resp
  where pullHeaders xs = do
          content <- pullHeadersValue xs "X-Server-Management-Url"
          cdn <- pullHeadersValue xs "X-CDN-Management-Url"
          token <- pullHeadersValue xs "X-Auth-Token"
          return $ Authorization (cdn, content, token)
        pullHeadersValue ((k,v):_) z | k == z = Just v
        pullHeadersValue (_:xs) z = pullHeadersValue xs z
        pullHeadersValue [] _ = Nothing
