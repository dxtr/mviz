module Mviz.Window.Events where

import           Data.Word (Word32)
import qualified SDL

data Event
  = Quit
  | ToggleUI
  | ToggleFullscreen
  | IgnoredEvent !SDL.EventPayload
  deriving (Show, Eq)
