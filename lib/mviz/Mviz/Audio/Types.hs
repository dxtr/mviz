{-# LANGUAGE AllowAmbiguousTypes #-}
module Mviz.Audio.Types
  ( AudioError(..)
  , MonadJack (..)
  , HasAudioClient (..)
  , JackReturnType
  , ServerAudioMessage (..)
  , ClientAudioMessage (..)
  ) where

import           Control.Concurrent.STM              (TQueue)
import           Control.Exception                   (Exception)
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Data.Text                           as T
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Exception                as JACKE

type JackReturnType a = Sync.ExceptionalT JACKE.All IO a

data AudioError
  = JACKError String
  deriving (Show)

-- Messages directed to the client
data ClientAudioMessage
  = Port String -- Inform the client about a port
  | SampleRate Int -- Inform the client about the sample rate
  | BufferSize Int -- Inform the client about the buffer size
  deriving (Show)

-- Messages directed to the server
data ServerAudioMessage
  = Quit -- Tell the server to quit
  | GetSampleRate
  | GetBufferSize
  deriving (Show)

instance Exception AudioError

class Monad m => MonadJack m where
  jackAction :: JackReturnType a -> m a
  activateClient :: m ()
  deactivateClient :: m ()
--   closeClient :: m ()
  ports :: m [T.Text]
  bufferSize :: m Int
  sampleRate :: m Int

class HasAudioClient a where
  getAudioClient :: a -> JACK.Client

class HasRecvChannel a where
  getRecvChannel :: a -> TQueue ServerAudioMessage
