module Network.Rackspace.CloudAPI.IO
  () where

import qualified Data.ByteString.Lazy as B
import Data.Convertible (convert)
import Data.Digest.Pure.MD5
import Data.String.Utils (split)
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Network.Curl
import Network.Rackspace.CloudAPI

instance CloudAPI IO where
  --getAuthorization :: (AccountName, AccountAPIKey) -> IO (Maybe Authorization)
  getAuthorization (account, key) = do
    let headers = ["X-Auth-User: " ++ account, "X-Auth-Key: " ++ key]
    resp <- curlGetResponse "https://auth.api.rackspacecloud.com/v1.0" [CurlHttpHeaders headers]
    return $ pullHeaders $ respHeaders resp
    where pullHeaders xs = do
            storage <- lookup "X-Storage-Url" xs
            cdn <- lookup "X-CDN-Management-Url" xs
            token <- lookup"X-Auth-Token" xs
            return $ Authorization (cdn, storage, token)

  --getContainers :: Authorization -> IO [Container]
  getContainers auth = do
    let Authorization (_, content_url, token) = auth
    let headers = [ "X-Auth-Token: " ++ token ]
    let url = content_url
    resp <- curlGetResponse url [CurlHttpHeaders headers]
    return $ filter ((/=) "" ) $ split "\n" $ respBody resp

  --createContainer :: Authorization -> String -> IO Bool
  createContainer auth container_name = do
    let Authorization (_, content_url, token) = auth
    let headers = [ "X-Auth-Token: " ++ token ]
    let url = content_url ++ "/" ++ container_name
    resp <- curlGetResponse url [CurlHttpHeaders headers,
                                 CurlPut True]
    return (respStatus resp == 201)

  --createFile :: Authorization -> Container -> String -> B.ByteString -> [(String, String)] -> IO Bool
  createFile auth container file_name file_contents meta_data = do
    let Authorization (_, content_url, token) = auth
    let md5_digest = show $ md5 file_contents
    let headers = [ "X-Auth-Token: " ++ token,
                    "ETag: " ++ md5_digest,
                    "Content-Type: image/x-canon-cr2" ]
                    ++ map formatMetaData meta_data
    let url = content_url ++ "/" ++ container ++ "/" ++ file_name
    state <- newIORef (0 :: Integer)
    resp <- curlGetResponse url [CurlHttpHeaders headers,
                                 CurlPut True,
                                 CurlUpload True,
                                 CurlReadFunction $ readFunction file_contents state ]
    return (respStatus resp == 201)
    where formatMetaData (a,b) = ("X-Object-Meta-" ++ a ++ ": " ++ b)

  --getFileList :: Authorization -> Container -> String -> IO [ String ]
  getFileList auth container prefix = do
    let Authorization (_, content_url, token) = auth
    let headers = [ "X-Auth-Token: " ++ token ]
    let url = content_url ++ "/" ++ container ++ "?path=" ++ prefix
    resp <- curlGetResponse url [CurlHttpHeaders headers]
    let contents = respBody resp
    return $ filter ((/=) "") $ split "\n" contents

-- Helper function for reading byte string into ptr.
readFunction :: B.ByteString -> IORef Integer -> ReadFunction
readFunction dat state = (\ptr width num _ -> do
  bytes_already_read <- readIORef state
  let width' = toInteger width
  let num' = toInteger num
  let bytes_to_copy = min ((width' * num')) ((toInteger $ B.length dat) - bytes_already_read)
  writeBS' dat ptr bytes_to_copy bytes_already_read
  writeIORef state (bytes_already_read + bytes_to_copy)
  return $ Just $ convert bytes_to_copy)
  where writeBS' _ _ 0 _ = return ()
        writeBS' bs ptr bytes i = do
          poke (castPtr ptr) $ B.index bs (fromInteger i)
          writeBS' bs (plusPtr ptr 1) (bytes-1) (i+1)
