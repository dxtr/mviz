{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

module ImGui.Raw.Structs
  ( Vec2(..)
  , Vec4(..)
  , ImVec2Ptr
  , ImVec4Ptr
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Control.Monad (liftM)

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include "cimgui.h"

data Vec2 = Vec2 !CFloat !CFloat deriving (Show, Eq, Read, Ord)
data Vec4 = Vec4 !CFloat !CFloat !CFloat !CFloat deriving (Show, Eq, Read, Ord)

{#pointer *ImVec2 as ImVec2Ptr -> Vec2 #}
{#pointer *ImVec4 as ImVec4Ptr -> Vec4 #}

instance Storable Vec2 where
  sizeOf _ = {#sizeof ImVec2#}
  alignment _ = {#alignof ImVec2 #}
  peek :: Ptr Vec2 -> IO Vec2
  peek p = Vec2
    <$> liftM id ({#get ImVec2->x #} p)
    <*> liftM id ({#get ImVec2->y #} p)
  poke :: Ptr Vec2 -> Vec2 -> IO ()
  poke p (Vec2 x y) = do
    {#set ImVec2.x #} p x
    {#set ImVec2.y #} p y

instance Storable Vec4 where
  sizeOf _ = {#sizeof ImVec4#}
  alignment _ = {#alignof ImVec4 #}
  peek :: Ptr Vec4 -> IO Vec4
  peek p = Vec4
    <$> liftM id ({#get ImVec4->x #} p)
    <*> liftM id ({#get ImVec4->y #} p)
    <*> liftM id ({#get ImVec4->z #} p)
    <*> liftM id ({#get ImVec4->w #} p)
  poke :: Ptr Vec4 -> Vec4 -> IO ()
  poke p (Vec4 x y z w) = do
    {#set ImVec4.x #} p x
    {#set ImVec4.y #} p y
    {#set ImVec4.z #} p z
    {#set ImVec4.w #} p w
