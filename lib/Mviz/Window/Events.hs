module Mviz.Window.Events where

import SDL (InputMotion (..))
import SDL qualified

data Event
  = Quit
  | WindowResized Int32 Int32

