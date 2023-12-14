module Mviz.Utils.Ringbuffer
  ( Ringbuffer
  , make
  , empty
  , put
  , toList
  , toVector) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable          (foldl')
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)
import           Data.List              (genericReplicate)
import           Data.Maybe             (catMaybes)
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as MV

data Ringbuffer a = Ringbuffer
  { ringIndex  :: !(IORef Int)
  , ringSize   :: !Word
  , ringBuffer :: !(MV.MVector (MV.PrimState IO) (Maybe a))
  }

currentIndex :: (MonadIO m) => Ringbuffer a -> m Int
currentIndex Ringbuffer{ringIndex = idx} = liftIO $ readIORef idx

nextIndex :: (MonadIO m) => Ringbuffer a -> m Int
nextIndex rb = nextIndex' rb <$> currentIndex rb

nextIndex' :: Ringbuffer a -> Int -> Int
nextIndex' Ringbuffer{ringSize = rsize} idx = (idx + 1) `mod` fromIntegral rsize

previousIndex' :: Ringbuffer a -> Int -> Int
previousIndex' Ringbuffer{ringSize = rsize} idx = (idx - 1) `mod` fromIntegral rsize

updateIndex :: (MonadIO m) => Ringbuffer a -> m Int
updateIndex ringbuffer@Ringbuffer{ringIndex = idx} =
  nextIndex ringbuffer >>= \nextIdx ->
    liftIO $ atomicModifyIORef' idx (const (nextIdx, nextIdx))

make_ :: (MonadIO m) => [Maybe a] -> m (Ringbuffer a)
make_ items = do
  idx <- liftIO $ newIORef 0
  buf <- liftIO . V.thaw $ V.fromList items
  -- buf <- mapM newIORef items
  pure $ Ringbuffer { ringIndex = idx
                      , ringSize = foldl' (\c _ -> c + 1) 0 items
                      , ringBuffer = buf
                      }

make :: (MonadIO m) => [a] -> m (Ringbuffer a)
make items = make_ $ map Just items

empty :: (MonadIO m) => Word -> m (Ringbuffer a)
empty size = make_ $ genericReplicate size Nothing

put :: (MonadIO m) => Ringbuffer a -> a -> m ()
put ringBuffer@Ringbuffer{ringBuffer = buffer} newItem =
  nextIndex ringBuffer
  >>= \idx -> do
    liftIO $ MV.write buffer idx (Just newItem)
    _ <- updateIndex ringBuffer
    pure ()

toList' :: (MonadIO m) => Ringbuffer a -> Int -> Int -> [Maybe a] -> m [Maybe a]
toList' rb@Ringbuffer{ringBuffer = buffer} endIndex cIdx acc
  | endIndex == cIdx = do
    currItem <- liftIO $ MV.read buffer cIdx
    pure $ acc ++ [currItem]
  | otherwise = do
    currItem <- liftIO $ MV.read buffer cIdx
    toList' rb endIndex (previousIndex' rb cIdx) (acc ++ [currItem])

toList :: (MonadIO m) => Ringbuffer a -> m [a]
toList rb@Ringbuffer{} = do
  endIndex <- nextIndex rb
  cIdx <- currentIndex rb
  catMaybes <$> toList' rb endIndex cIdx []

toVector :: (MonadIO m) => Ringbuffer a -> m (V.Vector a)
toVector rb = V.reverse . V.fromList <$> toList rb
