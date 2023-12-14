module Mviz.Window.Events where

import           Data.Word (Word32)
import qualified SDL

data Event
  = Quit
  | KeyUpEvent
  | WindowResized !Word32 !Word32
  | IgnoredEvent !SDL.EventPayload
  deriving (Show, Eq)
