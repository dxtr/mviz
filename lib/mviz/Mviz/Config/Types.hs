module Mviz.Config.Types
  ( Config (..)
  ) where

import qualified Data.Text      as T
import           Toml.FromValue (FromValue (fromValue), parseTableFromValue,
                                 reqKey)
import           Toml.ToValue   (ToTable (toTable), ToValue (toValue),
                                 defaultTableToValue, table, (.=))

data Config = Config { configInputs :: ![T.Text]
                     , configShowUI :: !Bool
                     } deriving (Show)

instance FromValue Config where
    fromValue = parseTableFromValue (Config <$> reqKey "inputs" <*> reqKey "showUI")

instance ToValue Config where
    toValue = defaultTableToValue

instance ToTable Config where
    toTable cfg = table ["inputs" .= configInputs cfg, "showUI" .= configShowUI cfg]
