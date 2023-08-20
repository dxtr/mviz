{-# LANGUAGE DuplicateRecordFields #-}

module ImGui.Structs (ImVec2(..), ImVec3(..), ImVec4(..), ImGuiContext, ImFont, ImFontConfig, ImFontGlyphRangesBuilder, ImDrawList) where

import           Foreign (Ptr, Storable (..), castPtr, plusPtr)

data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float }
    deriving (Show)

data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)

data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} !Float }
  deriving (Show)

data ImGuiContext
data ImFont
data ImFontConfig
data ImFontGlyphRangesBuilder
data ImDrawList

instance Storable ImVec2 where
  sizeOf :: ImVec2 -> Int
  sizeOf ~ImVec2{x, y} = sizeOf x + sizeOf y

  alignment :: ImVec2 -> Int
  alignment _ = 0

  poke :: Ptr ImVec2 -> ImVec2 -> IO ()
  poke ptr ImVec2{ x, y } = do
    poke (castPtr ptr `plusPtr` (sizeOf x * 0)) x
    poke (castPtr ptr `plusPtr` sizeOf x) y

  peek :: Ptr ImVec2 -> IO ImVec2
  peek ptr = do
    x <- peek (castPtr ptr)
    y <- peek (castPtr ptr `plusPtr` sizeOf x)
    return ImVec2 { x, y }

instance Storable ImVec3 where
  sizeOf :: ImVec3 -> Int
  sizeOf ~ImVec3{x, y, z} = sizeOf x + sizeOf y + sizeOf z

  alignment :: ImVec3 -> Int
  alignment _ = 0

  poke :: Ptr ImVec3 -> ImVec3 -> IO ()
  poke ptr ImVec3{ x, y, z } = do
    poke (castPtr ptr `plusPtr` 0) x
    poke (castPtr ptr `plusPtr` sizeOf x) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z

  peek :: Ptr ImVec3 -> IO ImVec3
  peek ptr = do
    x <- peek (castPtr ptr)
    y <- peek (castPtr ptr `plusPtr` sizeOf x)
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    return ImVec3{ x, y, z }

instance Storable ImVec4 where
  sizeOf :: ImVec4 -> Int
  sizeOf ~ImVec4{x, y, z, w} = sizeOf x + sizeOf y + sizeOf z + sizeOf w

  alignment :: ImVec4 -> Int
  alignment _ = 0

  poke :: Ptr ImVec4 -> ImVec4 -> IO ()
  poke ptr ImVec4{ x, y, z, w } = do
    poke (castPtr ptr `plusPtr` 0) x
    poke (castPtr ptr `plusPtr` sizeOf x) y
    poke (castPtr ptr `plusPtr` (sizeOf x * 2)) z
    poke (castPtr ptr `plusPtr` (sizeOf x * 3)) w

  peek :: Ptr ImVec4 -> IO ImVec4
  peek ptr = do
    x <- peek (castPtr ptr)
    y <- peek (castPtr ptr `plusPtr` sizeOf x)
    z <- peek (castPtr ptr `plusPtr` (sizeOf x * 2))
    w <- peek (castPtr ptr `plusPtr` (sizeOf x * 3))
    return ImVec4{ x, y, z, w }

