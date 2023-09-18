module ImGui.Raw.Types
  ( ImGuiListClipper
  , ListClipper
  , ImGuiID
  , ImGuiItemFlags
  , ImGuiItemStatusFlags
  ) where

import           Foreign.C.Types (CInt)
import           Foreign.Ptr     (Ptr)
-- import           ImGui.Raw.Structs

data ImGuiListClipper
type ListClipper = Ptr ImGuiListClipper
type ImGuiItemFlags = CInt
type ImGuiItemStatusFlags = CInt

type ImGuiID = Word
-- data LastItemData =
--   LastItemData { lastItemDataId      :: GuiID
--                , lastItemInFlags     :: [ItemFlag]
--                , lastItemRect        :: Rect
--                , lastItemNavRect     :: Rect
--                , lastItemDisplayRect :: Rect
--                }

