module ImGui.Frame
  ( newFrame
  , endFrame
  , withFrame
  ) where

import           Control.Exception (bracket_)
import qualified ImGui.Raw         as Raw (endFrame, newFrame)

newFrame :: IO ()
newFrame = Raw.newFrame

endFrame :: IO ()
endFrame = Raw.endFrame

withFrame :: IO b -> IO b
withFrame = bracket_ newFrame endFrame
