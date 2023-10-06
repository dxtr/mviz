module Mviz.Utils
  ( (<&&>)
  , whileM
  , whileM'
  , whileM_
  , untilM
  , untilM'
  , untilM_
  ) where

import           Control.Monad (MonadPlus, liftM, liftM2, mplus, mzero, when)

(<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

whileM :: Monad m => m Bool -> m a -> m [a]
whileM = whileM'

whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x  <- f
                        xs <- go
                        return (return x `mplus` xs)
                else return mzero

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x $ f >> go

untilM :: Monad m => m a -> m Bool -> m [a]
untilM = untilM'

untilM' :: (Monad m, MonadPlus f) => m a -> m Bool -> m (f a)
f `untilM'` p = do
        x  <- f
        xs <- whileM' (fmap not p) f
        return (return x `mplus` xs)

untilM_ :: (Monad m) => m a -> m Bool -> m ()
f `untilM_` p = f >> whileM_ (fmap not p) f
