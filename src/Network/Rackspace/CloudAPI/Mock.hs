{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Rackspace.CloudAPI.Mock
 (
   MockCloud
 ) where

import Control.Monad.State
import qualified Data.ByteString.Lazy as B
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Network.Rackspace.CloudAPI

type MockCloud = Map.Map Container (Map.Map String (B.ByteString, MetaData))

instance CloudAPI (State MockCloud) where
  getAuthorization _ = return $ Just $ Authorization ("", "", "")

  getContainers _ = do
    s <- get
    return $ Map.keys s

  createContainer _ container = do
    s <- get
    case Map.member container s of
      True -> return False
      False -> do
        put $ Map.insert container Map.empty s
        return True

  createFile _ container file_name file_contents meta_data = do
    s <- get
    case Map.lookup container s of
      Nothing -> return False
      Just c -> do
        let c' = Map.insert file_name (file_contents, meta_data) c
        put $ Map.insert container c' s
        return True
  
  getFileList _ cId p = do
    s <- get
    case Map.lookup cId s of
      Nothing -> return []
      Just c -> return $ filter (f p) $ Map.keys c
      where f p s = case stripPrefix p s of
              Nothing -> False
              _ -> True
