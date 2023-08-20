{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module ImGui.Raw.Context where

import qualified Data.Map.Strict           as Map
import           ImGui.Enums               (ImGuiSelectableFlag)
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
      , ( TypeName "ImGuiSelectableFlags", [t| ImGuiSelectableFlag |])
--      , ( TypeName "ImGuiListClipper", [t| ImGuiListClipper |] )
--      , ( TypeName "ImGuiTableSortSpecs", [t| ImGuiTableSortSpecs |] )
      ]
  }
