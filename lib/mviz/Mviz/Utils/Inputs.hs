{-# LANGUAGE OverloadedStrings #-}

module Mviz.Utils.Inputs (splitInputs, combinePortNames) where

import qualified Data.Text as T

splitInputs :: [T.Text] -> Maybe (T.Text, [T.Text])
splitInputs [] = Nothing
splitInputs inputs = foldr go Nothing split
  where split = map (T.splitOn ":") inputs
        go [] acc                   = acc
        go (input:channels) Nothing = Just (input, channels)
        go (input:channels) (Just (accInput, accChannels))
          | accInput == input = Just (input, accChannels <> channels)
          | otherwise = error "cannot have multiple different inputs"

-- TODO: Should probably take a (T.Text, [T.Text]) as input to be consistent
-- with splitInputs
combinePortNames :: T.Text -> [T.Text] -> [T.Text]
combinePortNames input channels = zipWith (\i c -> T.intercalate ":" [i, c]) (replicate (length channels) input) channels
