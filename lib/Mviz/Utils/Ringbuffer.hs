module Mviz.Utils.Ringbuffer (Ringbuffer, make, empty, put, next) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Data.Sequence qualified as S

data Ringbuffer a = Ringbuffer
  { ringWriterIndex :: TVar Int
  , ringReaderIndex :: TVar Int
  , ringSize :: Int -- This is static so doesn't have to be a TVar
  , ringBuffer :: S.Seq (TVar (Maybe a)) -- Only the items are mutable, not the buffer itself
  }

nextIndex :: Ringbuffer a -> Int -> Int
nextIndex Ringbuffer{ringSize = size} idx = (idx + 1) `mod` size

make :: [a] -> IO (Ringbuffer a)
make items = do
  wIdx <- newTVarIO 0
  rIdx <- newTVarIO 0
  buf <- mapM (newTVarIO . Just) items
  return $
    Ringbuffer
      { ringWriterIndex = wIdx
      , ringReaderIndex = rIdx
      , ringSize = length items
      , ringBuffer = S.fromList buf
      }

empty :: Int -> IO (Ringbuffer a)
empty size = do
  wIdx <- newTVarIO 0
  rIdx <- newTVarIO 0
  buf <- mapM newTVarIO $ replicate size Nothing
  return $
    Ringbuffer
      { ringWriterIndex = wIdx
      , ringReaderIndex = rIdx
      , ringSize = size
      , ringBuffer = S.fromList buf
      }

put :: Ringbuffer a -> a -> IO ()
put ringBuffer@Ringbuffer{ringBuffer = buffer, ringWriterIndex = wIndex} newItem = atomically $ do
  wIndex_ <- stateTVar wIndex (\idx -> (idx, nextIndex ringBuffer idx))
  let bufItem = S.index buffer wIndex_
  writeTVar bufItem $ Just newItem

next :: Ringbuffer a -> IO (Maybe a)
next ringBuffer@Ringbuffer{ringBuffer = buffer, ringReaderIndex = rIndex} = atomically $ do
  rIndex_ <- stateTVar rIndex (\idx -> (idx, nextIndex ringBuffer idx))
  readTVar $ S.index buffer rIndex_