{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

module ImGui.Raw.Types
  ( WindowFlag(..)
  , SelectableFlag(..)
  , Context(..)
  , GuiIO(..)
  , FontAtlas
  , FontAtlasPtr
  , DrawData(..)
  , Style
  , StylePtr
  , combineWindowFlags
  , combineSelectableFlags
  ) where

import           Foreign

#include "cimgui.h"

data FontAtlas
data Style

{#pointer *ImGuiContext as Context newtype #}
{#pointer *ImFontAtlas as FontAtlasPtr -> FontAtlas #}
{#pointer *ImGuiIO as GuiIO newtype #}
{#pointer *ImDrawData as DrawData newtype #}
{#pointer *ImGuiStyle as StylePtr -> Style #}

{#enum ImGuiWindowFlags_ as WindowFlag {underscoreToCase} deriving (Show, Eq) #}
{#enum ImGuiSelectableFlags_ as SelectableFlag {underscoreToCase} deriving (Show, Eq) #}

combineWindowFlags :: (Integral a) => [WindowFlag] -> a
combineWindowFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

combineSelectableFlags :: (Integral a) => [SelectableFlag] -> a
combineSelectableFlags = fromIntegral . foldr ((.|.) . fromEnum) 0
