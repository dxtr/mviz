{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
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
  , HasInputPorts(..)
  , InputPort(..)
  , HasSamples(..)
  , AudioException (..)
  , HasPortLock(..)
  ) where

import           Control.Concurrent                  (MVar)
import           Control.Concurrent.STM              (TQueue)
import           Control.Exception                   (Exception)
import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Maybe           (MaybeT)
import           Data.IORef                          (IORef)
import qualified Data.Text                           as T
import           Foreign                             (Ptr)
import           GHC.Stack                           (HasCallStack, callStack,
                                                      prettyCallStack)
import           Mviz.Audio.Inputs                   (InputMap)
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as JACKA
import qualified Sound.JACK.Exception                as JACKE

type JackReturnType a = Sync.ExceptionalT JACKE.All IO a

-- newtype InputPort = InputPort (JACK.Port CFloat JACK.Input)
data InputPort = InputPort { inputPortName   :: T.Text
                           , inputPortHandle :: JACK.Port JACKA.Sample JACK.Input
                           , inputPortTarget :: T.Text
                           }

data AudioError =
  AudioError { audioErrorMessage :: String
             }

instance Show AudioError where
  show = audioErrorMessage

data AudioException a where
  AudioException :: (Show a, HasCallStack) => a -> AudioException a

instance Show (AudioException a) where
  show (AudioException e) = "AudioException\n" <> show e <> "\n" <> prettyCallStack callStack

-- instance Exception AudioError
instance Exception (AudioException AudioError)

-- deriving anyclass instance Exception (AudioException AudioError)

-- Messages directed to the client
data ClientAudioMessage
  = Ports [T.Text]
  | Inputs InputMap
  | SampleRate Word -- Inform the client about the sample rate
  | BufferSize Word -- Inform the client about the buffer size
  | Samples [Float]
  deriving (Show)

-- Messages directed to the server
data ServerAudioMessage
  = Quit -- Tell the server to quit
  | GetSamples
  | GetSampleRate
  | GetBufferSize
  | GetInputs
  | SetInput (T.Text, [T.Text])
  deriving (Show)

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
  newPort :: T.Text -> T.Text -> m InputPort
  disposePort :: InputPort -> m ()
  setPorts :: [InputPort] -> m ()
  getPorts :: m [InputPort]
  portName :: InputPort -> m T.Text
  connectPorts :: [InputPort] -> m ()
  disconnectPorts :: m ()
  isPortConnected :: InputPort -> m Bool
  setProcessCallback :: JACK.Process a -> Ptr a -> m ()
  sampleBufferRef :: m (IORef [[JACKA.Sample]])
  sampleBuffer :: m [[JACKA.Sample]]
  withPortLock :: m a -> m a

class Monad m => MonadAudioServer m where
  serverRecvChannel :: m (TQueue ServerAudioMessage)
  serverSendChannel :: m (TQueue ClientAudioMessage)
  serverRecvMessage :: m ServerAudioMessage
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

class HasPortLock a where
  getPortLock :: a -> MVar ()

class HasInputPorts a where
  getInputPorts :: a -> IO [InputPort]
  setInputPorts :: a -> [InputPort] -> IO ()
-- class HasPorts a where
--  getPortsRef :: a -> IORef [T.Text]

class HasInputs a where
  getPortsRef :: a -> IORef [T.Text]
  getInputsRef :: a -> IORef InputMap

class HasSamples a where
  getSampleBufferRef :: a -> IORef [[JACKA.Sample]]

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
