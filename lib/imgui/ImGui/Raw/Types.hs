module ImGui.Raw.Types
  ( ImGuiListClipper
  , ListClipper
  ) where

import           Foreign.Ptr (Ptr)

data ImGuiListClipper
type ListClipper = Ptr ImGuiListClipper
