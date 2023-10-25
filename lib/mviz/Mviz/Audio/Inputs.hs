{-# LANGUAGE OverloadedStrings #-}


module Mviz.Audio.Inputs
  ( InputMap
  , mkInputMap
  , getInputs
  , getChannels
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T

newtype InputMap = InputMap (M.Map T.Text (S.Set T.Text))
    deriving (Show)

splitInput :: T.Text -> (T.Text, T.Text)
splitInput input =
    let (i,c) = T.breakOn ":" input in
        (i, T.dropWhile (== ':') c)

insertInMap :: InputMap -> (T.Text, T.Text) -> InputMap
insertInMap (InputMap im) (input,channel) =
    InputMap $ M.insertWith S.union input (S.singleton channel) im

mkInputMap :: [T.Text] -> InputMap
mkInputMap = foldl insertInMap (InputMap M.empty) . map splitInput

getInputs :: InputMap -> [T.Text]
getInputs (InputMap m) = M.keys m

getChannels :: InputMap -> T.Text -> [T.Text]
getChannels (InputMap m) input = maybe [] S.elems (M.lookup input m)
