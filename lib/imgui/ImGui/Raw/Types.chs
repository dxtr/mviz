{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

module ImGui.Raw.Types
  ( WindowFlag(..)
  , SelectableFlag(..)
  , Context(..)
  , GuiIO(..)
  , FontAtlas(..)
  , DrawData(..)
  , combineWindowFlags
  , combineSelectableFlags
  ) where

import           Foreign
import Foreign.C.Types (CInt)

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include "cimgui.h"

-- type ImGuiContext = Ptr ()
{#pointer *ImGuiContext as Context newtype #}
{#pointer *ImFontAtlas as FontAtlas newtype #}
{#pointer *ImGuiIO as GuiIO newtype #}
{#pointer *ImDrawData as DrawData newtype #}

{#enum ImGuiWindowFlags_ as WindowFlag {underscoreToCase} deriving (Show, Eq) #}
{#enum ImGuiSelectableFlags_ as SelectableFlag {underscoreToCase} deriving (Show, Eq) #}

combineWindowFlags :: (Integral a) => [WindowFlag] -> a
combineWindowFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

combineSelectableFlags :: (Integral a) => [SelectableFlag] -> a
combineSelectableFlags = fromIntegral . foldr ((.|.) . fromEnum) 0
