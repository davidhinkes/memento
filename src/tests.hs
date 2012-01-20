--module Tests () where

import Data.String
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Util

t1 = TestCase (assertEqual "" str (bs2s bs))
  where str = "my string"
        bs = (fromString str) :: B.ByteString

t2 = TestCase (assertEqual "" str (lbs2s bs))
  where str = "my string"
        bs = (fromString str) :: LB.ByteString

tests = TestList [TestLabel "bs2s test" t1,
                  TestLabel "lbs2s test" t2]

main = do
  runTestTT tests
  return ()
