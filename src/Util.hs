module Util (bs2s, lbs2s) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

bs2s :: BS.ByteString -> String
bs2s = BS8.unpack

lbs2s :: LBS.ByteString -> String
lbs2s = LBS8.unpack
