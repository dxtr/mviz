module ImGui.Frame
  ( newFrame
  , endFrame
  , withFrame
  ) where

import           Control.Exception (bracket)
import qualified ImGui.Raw         as Raw (endFrame, newFrame)

newFrame :: IO ()
newFrame = Raw.newFrame

endFrame :: IO ()
endFrame = Raw.endFrame

withFrame :: (a -> IO b) -> IO b
withFrame = bracket newFrame (\_ -> endFrame)
