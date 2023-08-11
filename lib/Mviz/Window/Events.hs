module Mviz.Window.Events where

import Data.Word (Word32)
import SDL qualified

data Event
  = Quit
  | WindowResized Word32 Word32
  | IgnoredEvent SDL.EventPayload
  deriving (Show, Eq)
