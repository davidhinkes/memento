import Data.Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.String
import Control.Monad.State
import Memento (archive)
import Network.Rackspace.CloudAPI.Mock
import Test.HUnit
import Util

t1 = TestCase (assertEqual "" str (bs2s bs))
  where str = "my string"
        bs = (fromString str) :: BS.ByteString

t2 = TestCase (assertEqual "" str (lbs2s bs))
  where str = "my string"
        bs = (fromString str) :: LBS.ByteString

archiveTest = TestCase $ assertBool "" (hasCorrectFile)
  where c = archive "_id_" fileContents :: Control.Monad.State.State MockCloud ()
        s = execState c Map.empty
        fileContents = fromString "test" :: LBS.ByteString
        hasCorrectFile = Just (fileContents, []) ==  do
          container <- Map.lookup "memento" s
          Map.lookup "_id_/098f6bcd4621d373cade4e832627b4f6" container

tests = TestList [TestLabel "bs2s test" t1,
                  TestLabel "lbs2s test" t2,
                  TestLabel "archive test" archiveTest ]

main = do
  runTestTT tests
  return ()
