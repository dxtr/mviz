module Mviz.Utils.Ringbuffer (Ringbuffer, make, empty, put, next, toList) where

import           Data.Foldable (foldl')
import qualified Data.Foldable as F
import           Data.IORef    (IORef, atomicModifyIORef', atomicWriteIORef,
                                newIORef, readIORef)
import           Data.Maybe    (catMaybes)
import qualified Data.Sequence as S

data Ringbuffer a = Ringbuffer
  { ringWriterIndex :: IORef Word
  , ringReaderIndex :: IORef Word
  , ringSize        :: Word -- This is static so doesn't have to be a TVar
  , ringBuffer      :: S.Seq (IORef (Maybe a)) -- Only the items are mutable, not the buffer
  }

nextIndex :: Ringbuffer a -> Word -> Word
nextIndex Ringbuffer{ringSize = size} idx = (idx + 1) `mod` size

make_ :: [Maybe a] -> IO (Ringbuffer a)
make_ items = do
  wIdx <- newIORef 0
  rIdx <- newIORef 0
  buf <- mapM newIORef items
  return $ Ringbuffer { ringWriterIndex = wIdx
                      , ringReaderIndex = rIdx
                      , ringSize = foldl' (\c _ -> c + 1) 0 items
                      , ringBuffer = S.fromList buf
                      }

make :: [a] -> IO (Ringbuffer a)
make items = make_ $ map Just items

empty :: Int -> IO (Ringbuffer a)
empty size = make_ $ replicate size Nothing

put :: Ringbuffer a -> a -> IO ()
put ringBuffer@Ringbuffer{ringBuffer = buffer, ringWriterIndex = wIndex} newItem =
  S.index buffer <$> atomicModifyIORef' wIndex updateIndexFunc >>= ((flip atomicWriteIORef) $ Just newItem)
 where
  updateIndexFunc idx = (nextIndex ringBuffer idx, fromIntegral idx)

next :: Ringbuffer a -> IO (Maybe a)
next ringBuffer@Ringbuffer{ringBuffer = buffer, ringReaderIndex = rIndex} =
  readIORef =<< S.index buffer <$> atomicModifyIORef' rIndex updateIndexFunc
 where
  updateIndexFunc idx = (nextIndex ringBuffer idx, fromIntegral idx)

toList :: Ringbuffer a -> IO [a]
toList Ringbuffer{ringBuffer = buffer} = catMaybes . F.toList <$> traverse readIORef buffer
