module Mviz.Config.Types
  ( Config (..)
  ) where

import qualified Data.Text      as T
import           Toml.FromValue (FromValue (fromValue), parseTableFromValue,
                                 reqKey)

data Config = Config { configInputs :: [T.Text]
                     , configShowUI :: Bool
                     } deriving (Show)

instance FromValue Config where
    fromValue = parseTableFromValue (Config <$> reqKey "inputs" <*> reqKey "showUI")
