module Mviz.Utils
  ( (<&&>)
  , if'
  ) where

import           Control.Monad (liftM2)

(<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
