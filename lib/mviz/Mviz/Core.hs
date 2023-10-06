module Mviz.Core
  (Mviz (..)
  ) where

import           Control.Monad           (ap)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Data.Functor            ((<&>))
import           Mviz.Types

newtype Mviz a = Mviz
  { unMviz :: MvizEnvironment -> IO a
  }

instance Functor Mviz where
  fmap f a = a <&> f

instance Applicative Mviz where
  pure = Mviz . const . pure
  (<*>) = ap

instance Monad Mviz where
  return = pure
  Mviz x >>= f = Mviz $ \y -> do
    a <- x y
    unMviz (f a) y

instance MonadIO Mviz where
  liftIO = Mviz . const

instance MonadUnliftIO Mviz where
  withRunInIO inner = Mviz $ \x -> inner $ flip unMviz x
