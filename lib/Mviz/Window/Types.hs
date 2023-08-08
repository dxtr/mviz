module Mviz.Window.Types (WindowMode (..), Size (..)) where

data WindowMode = Fullscreen | FullscreenDesktop | Windowed
data Size = Size
    { sizeWidth :: Int
    , sizeHeight :: Int
    }
    deriving (Show)
