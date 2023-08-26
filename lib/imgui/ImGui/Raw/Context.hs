{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module ImGui.Raw.Context where

import qualified Data.Map.Strict           as Map
import           Foreign.C.Types           (CInt)
import           ImGui.Raw.Types           (ImGuiListClipper)
import           ImGui.Structs
import           Language.C.Inline.Context (Context (..))
import           Language.C.Types          (pattern TypeName)

imguiContext :: Context
imguiContext = mempty
  { ctxTypesTable = Map.fromList
      [ ( TypeName "ImVec2", [t| ImVec2 |] )
      , ( TypeName "ImVec3", [t| ImVec3 |] )
      , ( TypeName "ImVec4", [t| ImVec4 |] )
--      , ( TypeName "ImU32", [t| ImU32 |] )
--      , ( TypeName "ImGuiID", [t| ImGuiID |] )
--      , ( TypeName "ImWchar", [t| ImWchar |] )
      , ( TypeName "ImDrawList", [t| ImDrawList |] )
      , ( TypeName "ImGuiContext", [t| ImGuiContext |] )
      , ( TypeName "ImFont", [t| ImFont |] )
      , ( TypeName "ImFontConfig", [t| ImFontConfig |] )
      , ( TypeName "ImFontGlyphRangesBuilder", [t| ImFontGlyphRangesBuilder |] )
      , ( TypeName "ImGuiSelectableFlags", [t| CInt |])
      , ( TypeName "ImGuiWindowFlags", [t| CInt |])
      , ( TypeName "ImGuiListClipper", [t| ImGuiListClipper |] )
--      , ( TypeName "ImGuiTableSortSpecs", [t| ImGuiTableSortSpecs |] )
      ]
  }
