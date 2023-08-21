module ImGui.Types ( DrawData(..)
                   ) where

import           Foreign.Ptr (Ptr)

newtype DrawData = DrawData (Ptr ())
