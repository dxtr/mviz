module Mviz.Utils.TSRingbuffer (Ringbuffer, make, empty, put, next) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              stateTVar, writeTVar)
import           Data.Foldable               (foldl')
import qualified Data.Sequence               as S

data Ringbuffer a = Ringbuffer
  { ringWriterIndex :: TVar Word
  , ringReaderIndex :: TVar Word
  , ringSize        :: Word -- This is static so doesn't have to be a TVar
  , ringBuffer      :: S.Seq (TVar (Maybe a)) -- Only the items are mutable, not the buffer itself
  }

nextIndex :: Ringbuffer a -> Word -> Word
nextIndex Ringbuffer{ringSize = size} idx = (idx + 1) `mod` size

make_ :: [Maybe a] -> IO (Ringbuffer a)
make_ items = do
  wIdx <- newTVarIO 0
  rIdx <- newTVarIO 0
  buf <- mapM newTVarIO items
  return $
    Ringbuffer
      { ringWriterIndex = wIdx
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
  atomically $ S.index buffer <$> stateTVar wIndex updateIndexFunc >>= ((flip writeTVar) $ Just newItem)
 where
  updateIndexFunc idx = (fromIntegral idx, nextIndex ringBuffer idx)

next :: Ringbuffer a -> IO (Maybe a)
next ringBuffer@Ringbuffer{ringBuffer = buffer, ringReaderIndex = rIndex} =
  atomically $ readTVar =<< S.index buffer <$> stateTVar rIndex updateIndexFunc
 where
  updateIndexFunc idx = (fromIntegral idx, nextIndex ringBuffer idx)
