module Graphics.CR2
  (
  getCR2Attributes,
  ) where

import Data.Maybe
import Data.Serialize.Get
import Data.Word
import Util

data Endian = BigEndian | LittleEndian

e8 :: Endian -> Get Word8
e8 _ = getWord8

e16 :: Endian -> Get Word16
e16 endian = case endian of
               BigEndian -> getWord16be
               LittleEndian -> getWord16le 

e32 :: Endian -> Get Word32
e32 endian = case endian of
               BigEndian -> getWord32be
               LittleEndian -> getWord32le 

getCR2Endian :: Get Endian
getCR2Endian = do
  endian_indicator <- getLazyByteString 2
  case (lbs2s endian_indicator) of
    "II" -> return LittleEndian
    "MM" -> return BigEndian
    _ -> fail $ "Expected endian indicator.  Got: " ++ (show endian_indicator)

getCR2RawOffset :: Endian -> Get Word32
getCR2RawOffset endian = do
  skip 4
  e32 endian

type CR2Tag = (Word16, Word16, Word32, Word32)

getCR2Tag :: Endian -> Get CR2Tag
getCR2Tag endian = do
  id' <- e16 endian
  id_type <- e16 endian
  num <- e32 endian
  value <- e32 endian
  return (id', id_type, num, value)

getCR2Tags :: Endian -> Get [CR2Tag]
getCR2Tags endian = do
  offset <- lookAhead $ getCR2RawOffset endian
  skip $ fromInteger $ toInteger $ offset
  entries <- e16 endian
  sequence $ take (fromInteger . toInteger $ entries) $ repeat (getCR2Tag endian)

type CR2Attribute = (String, String)

convertTagToAttribute :: CR2Tag -> Maybe (Get CR2Attribute)
convertTagToAttribute (306, _, n, offset) = Just $ do
  skip (fromInteger $ toInteger offset)
  date <- getLazyByteString $ fromInteger $ toInteger (n-1)
  return ("Date", lbs2s date)
convertTagToAttribute (271, _, n, offset) = Just $ do
  skip (fromInteger $ toInteger offset)
  make <- getLazyByteString $ fromInteger $ toInteger (n-1)
  return ("Make", lbs2s make)
convertTagToAttribute (272, _, n, offset) = Just $ do
  skip (fromInteger $ toInteger offset)
  make <- getLazyByteString $ fromInteger $ toInteger (n-1)
  return ("Model", lbs2s make)
convertTagToAttribute _ = Nothing
-- Nothing passes here.  The following can be used for debugging:
--  convertTagToAttribute (tag, 2, n, offset) = Just $ do
--    skip (fromInteger $ toInteger offset)
--    str <- getLazyByteString $ fromInteger $ toInteger (n-1)
--    return (show tag, lbs2s str)
--  convertTagToAttribute (tag, _, n, offset) = Just $ do
--    return (show tag, "DEBUG")

getCR2Attributes :: Get [CR2Attribute]
getCR2Attributes = do
  endian <- lookAhead getCR2Endian
  attribute_tags <- lookAhead $ getCR2Tags endian
  sequence $ map lookAhead $ mapMaybe (convertTagToAttribute) attribute_tags

