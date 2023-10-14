{-# LANGUAGE DuplicateRecordFields #-}

module ImGui.Raw.Structs
  ( ImVec2(..)
  , ImVec3(..)
  , ImVec4(..)
  , ImRect(..)
  , ImGuiLastItemData(..)
  , ImGuiContext
  , ImFont
  , ImFontConfig
  , ImFontGlyphRangesBuilder
  , ImDrawList
  ) where

import           Foreign         (Ptr, Storable (..), castPtr, plusPtr)
import           ImGui.Enums     ()
import           ImGui.Raw.Types (ImGuiID, ImGuiItemFlags, ImGuiItemStatusFlags)

data ImVec2 = ImVec2 { x, y :: {-# unpack #-} !Float }
    deriving (Show)

data ImVec3 = ImVec3 { x, y, z :: {-# unpack #-} !Float }
  deriving (Show)

data ImVec4 = ImVec4 { x, y, z, w :: {-# unpack #-} !Float }
  deriving (Show)

data ImRect = ImRect { rectMin, rectMax :: !ImVec2 }
  deriving (Show)

data ImGuiLastItemData =
  ImGuiLastItemData { lastItemID          :: ImGuiID
                    , lastItemFlags       :: ImGuiItemFlags
                    , lastItemStatusFlags :: ImGuiItemStatusFlags
                    , lastItemRect        :: ImRect
                    , lastItemNavRect     :: ImRect
                    , lastItemDisplayRect :: ImRect
                    }

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
    pure ImVec2 { x, y }

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
    pure ImVec3{ x, y, z }

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
    pure ImVec4{ x, y, z, w }

instance Storable ImRect where
  sizeOf :: ImRect -> Int
  sizeOf ImRect{rectMin, rectMax} = sizeOf rectMin + sizeOf rectMax

  alignment :: ImRect -> Int
  alignment _ = 0

  poke :: Ptr ImRect -> ImRect -> IO ()
  poke ptr ImRect{rectMin, rectMax} = do
    poke (castPtr ptr `plusPtr` 0) rectMin
    poke (castPtr ptr `plusPtr` sizeOf rectMin) rectMax

  peek :: Ptr ImRect -> IO ImRect
  peek ptr = do
    rectMin <- peek (castPtr ptr)
    rectMax <- peek (castPtr ptr `plusPtr` sizeOf rectMin)
    pure ImRect{ rectMin, rectMax}

instance Storable ImGuiLastItemData where
  sizeOf :: ImGuiLastItemData -> Int
  sizeOf ImGuiLastItemData{lastItemID, lastItemFlags, lastItemStatusFlags, lastItemRect, lastItemNavRect, lastItemDisplayRect} =
    sizeOf lastItemID +
    sizeOf lastItemFlags +
    sizeOf lastItemStatusFlags +
    sizeOf lastItemRect +
    sizeOf lastItemNavRect +
    sizeOf lastItemDisplayRect

  alignment :: ImGuiLastItemData -> Int
  alignment _ = 0

  poke :: Ptr ImGuiLastItemData -> ImGuiLastItemData -> IO ()
  poke ptr ImGuiLastItemData{lastItemID, lastItemFlags, lastItemStatusFlags, lastItemRect, lastItemNavRect, lastItemDisplayRect} = do
    poke (castPtr ptr `plusPtr` lastItemIDOffset) lastItemID
    poke (castPtr ptr `plusPtr` lastItemFlagsOffset) lastItemFlags
    poke (castPtr ptr `plusPtr` lastItemStatusFlagsOffset) lastItemStatusFlags
    poke (castPtr ptr `plusPtr` lastItemRectOffset) lastItemRect
    poke (castPtr ptr `plusPtr` lastItemNavRectOffset) lastItemNavRect
    poke (castPtr ptr `plusPtr` lastItemDisplayRectOffset) lastItemDisplayRect
      where lastItemIDOffset = 0
            lastItemFlagsOffset = lastItemIDOffset + sizeOf lastItemID
            lastItemStatusFlagsOffset = lastItemFlagsOffset + sizeOf lastItemFlags
            lastItemRectOffset = lastItemStatusFlagsOffset + sizeOf lastItemStatusFlags
            lastItemNavRectOffset = lastItemRectOffset + sizeOf lastItemRect
            lastItemDisplayRectOffset = lastItemNavRectOffset + sizeOf lastItemNavRect

  peek :: Ptr ImGuiLastItemData -> IO ImGuiLastItemData
  peek ptr = do
    let lastItemIDOffset = 0
    lastItemID <- peek (castPtr ptr)

    let lastItemFlagsOffset = lastItemIDOffset + sizeOf lastItemID
    lastItemFlags <- peek (castPtr ptr `plusPtr` lastItemFlagsOffset)

    let lastItemStatusFlagsOffset = lastItemFlagsOffset + sizeOf lastItemFlags
    lastItemStatusFlags <- peek (castPtr ptr `plusPtr` lastItemStatusFlagsOffset)

    let lastItemRectOffset = lastItemStatusFlagsOffset + sizeOf lastItemStatusFlags
    lastItemRect <- peek (castPtr ptr `plusPtr` lastItemRectOffset)

    let lastItemNavRectOffset = lastItemRectOffset + sizeOf lastItemRect
    lastItemNavRect <- peek (castPtr ptr `plusPtr` lastItemNavRectOffset)

    let lastItemDisplayRectOffset = lastItemNavRectOffset + sizeOf lastItemNavRect
    lastItemDisplayRect <- peek (castPtr ptr `plusPtr` lastItemDisplayRectOffset)
    pure ImGuiLastItemData{ lastItemID, lastItemFlags, lastItemStatusFlags, lastItemRect, lastItemNavRect, lastItemDisplayRect }







