module Main where

import Control.Applicative
import Data.String
import Memento
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = do
  quickHttpServe $ staticFiles <|> route [
    (fromString "upload", loginProtect (\r -> upload r)),
    (fromString "status", loginProtect (\r -> status r))
    ]
