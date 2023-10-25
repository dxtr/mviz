{-# LANGUAGE AllowAmbiguousTypes #-}
module Mviz.Audio.Types
  ( AudioError(..)
  , MonadJack (..)
  , HasAudioClient (..)
  , HasServerChannel (..)
  , HasClientChannel (..)
  , JackReturnType
  , ServerAudioMessage (..)
  , ClientAudioMessage (..)
  , MonadAudioServer (..)
  , MonadAudioClient (..)
  , HasBufferSize (..)
  , HasSampleRate (..)
  , MonadAudio (..)
  , HasInputs (..)
  ) where

import           Control.Concurrent.STM              (TQueue)
import           Control.Exception                   (Exception)
import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Maybe           (MaybeT)
import           Data.IORef                          (IORef)
import qualified Data.Text                           as T
import           Mviz.Audio.Inputs                   (InputMap)
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Exception                as JACKE

type JackReturnType a = Sync.ExceptionalT JACKE.All IO a

data AudioError
  = JACKError String
  deriving (Show)

-- Messages directed to the client
data ClientAudioMessage
  = Ports [T.Text]
  | Inputs InputMap
  | SampleRate Word -- Inform the client about the sample rate
  | BufferSize Word -- Inform the client about the buffer size
  deriving (Show)

-- Messages directed to the server
data ServerAudioMessage
  = Quit -- Tell the server to quit
  | GetSampleRate
  | GetBufferSize
  | GetInputs
  deriving (Show)

instance Exception AudioError

class Monad m => MonadAudio m where
  audioPorts :: m [T.Text]
  audioInputs :: m InputMap
  audioBufferSize :: m Word
  audioSampleRate :: m Word

class Monad m => MonadJack m where
  jackAction :: JackReturnType a -> m a
  ports :: m [T.Text]
  inputs :: m InputMap
  bufferSize :: m Word
  sampleRate :: m Word

class Monad m => MonadAudioServer m where
  serverRecvChannel :: m (TQueue ServerAudioMessage)
  serverSendChannel :: m (TQueue ClientAudioMessage)
  serverRecvMessage :: m (Maybe ServerAudioMessage)
  serverSendMessage :: ClientAudioMessage -> m ()

class Monad m => MonadAudioClient m where
  clientRecvChannel :: m (TQueue ClientAudioMessage)
  clientSendChannel :: m (TQueue ServerAudioMessage)
  clientRecvMessage :: m (Maybe ClientAudioMessage)
  clientRecvMessages :: m [ClientAudioMessage]
  clientSendMessage :: ServerAudioMessage -> m ()

class HasAudioClient a where
  getAudioClient :: a -> JACK.Client

class HasClientChannel a where
  getClientChannel :: a -> TQueue ClientAudioMessage

class HasServerChannel a where
  getServerChannel :: a -> TQueue ServerAudioMessage

class HasBufferSize a where
  getBufferSizeRef :: a -> IORef Word

class HasSampleRate a where
  getSampleRateRef :: a -> IORef Word

-- class HasPorts a where
--  getPortsRef :: a -> IORef [T.Text]

class HasInputs a where
  getPortsRef :: a -> IORef [T.Text]
  getInputsRef :: a -> IORef InputMap

-- class HasRecvChannel a where
--   getRecvChannel :: a -> TQueue ServerAudioMessage

-- class HasSendChannel a where
--   getSendChannel :: a -> TQueue ClientAudioMessage

instance (MonadAudioClient m) => MonadAudioClient (MaybeT m) where
  clientRecvChannel :: MonadAudioClient m => MaybeT m (TQueue ClientAudioMessage)
  clientRecvChannel = lift clientRecvChannel

  clientSendChannel :: MonadAudioClient m => MaybeT m (TQueue ServerAudioMessage)
  clientSendChannel = lift clientSendChannel

  clientRecvMessage :: MonadAudioClient m => MaybeT m (Maybe ClientAudioMessage)
  clientRecvMessage = lift clientRecvMessage

  clientRecvMessages :: MonadAudioClient m => MaybeT m [ClientAudioMessage]
  clientRecvMessages = lift clientRecvMessages

  clientSendMessage :: MonadAudioClient m => ServerAudioMessage -> MaybeT m ()
  clientSendMessage = lift . clientSendMessage
