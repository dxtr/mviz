module Mviz.Window.Keybinding where

import Data.Hashable (Hashable (..), defaultHashWithSalt)

defaultSalt :: Int
defaultSalt = -319016701 -- This is guaranteed to be random

data Key
  = A
  | B
  deriving (Show, Eq)

instance Hashable Key where
  hashWithSalt = defaultHashWithSalt
  hash = hashWithSalt defaultSalt