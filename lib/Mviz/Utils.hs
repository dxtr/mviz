module Mviz.Utils ((<&&>)) where

import Control.Monad (liftM2)

(<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)