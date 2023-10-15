{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module ImGui.Raw.Context where

import           Data.Int                  (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict           as Map
import           Data.Word                 (Word16, Word32, Word64)
import           Foreign                   (Word8)
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
      , ( TypeName "ImU8", [t| Word8 |] )
      , ( TypeName "ImU16", [t| Word16 |] )
      , ( TypeName "ImU32", [t| Word32 |] )
      , ( TypeName "ImU64", [t| Word64 |] )
      , ( TypeName "ImS8", [t| Int8 |] )
      , ( TypeName "ImS16", [t| Int16 |] )
      , ( TypeName "ImS32", [t| Int32 |] )
      , ( TypeName "ImS64", [t| Int64 |] )
      , ( TypeName "ImGuiID", [t| Word |] )
--      , ( TypeName "ImWchar", [t| ImWchar |] )
      , ( TypeName "ImDrawList", [t| ImDrawList |] )
      , ( TypeName "ImGuiContext", [t| ImGuiContext |] )
      , ( TypeName "ImFont", [t| ImFont |] )
      , ( TypeName "ImFontConfig", [t| ImFontConfig |] )
      , ( TypeName "ImFontGlyphRangesBuilder", [t| ImFontGlyphRangesBuilder |] )
      , ( TypeName "ImGuiSelectableFlags", [t| CInt |])
      , ( TypeName "ImGuiTreeNodeFlags", [t| CInt |])
      , ( TypeName "ImGuiWindowFlags", [t| CInt |])
      , ( TypeName "ImGuiListClipper", [t| ImGuiListClipper |] )
      , ( TypeName "ImRect", [t| ImRect |] )
      , ( TypeName "ImGuiLastItemData", [t| ImGuiLastItemData |] )

--      , ( TypeName "ImGuiTableSortSpecs", [t| ImGuiTableSortSpecs |] )
      ]
  }
