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
  ) where

import           Control.Concurrent.STM              (TQueue)
import           Control.Exception                   (Exception)
import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Maybe           (MaybeT)
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
  | Ports [T.Text]
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

class Monad m => MonadAudioServer m where
  serverRecvChannel :: m (TQueue ServerAudioMessage)
  serverSendChannel :: m (TQueue ClientAudioMessage)
  serverRecvMessage :: m (Maybe ServerAudioMessage)
  serverSendMessage :: ClientAudioMessage -> m ()

class Monad m => MonadAudioClient m where
  clientRecvChannel :: m (TQueue ClientAudioMessage)
  clientSendChannel :: m (TQueue ServerAudioMessage)
  clientRecvMessage :: m (Maybe ClientAudioMessage)
  clientSendMessage :: ServerAudioMessage -> m ()

class HasAudioClient a where
  getAudioClient :: a -> JACK.Client

class HasClientChannel a where
  getClientChannel :: a -> TQueue ClientAudioMessage

class HasServerChannel a where
  getServerChannel :: a -> TQueue ServerAudioMessage

-- class HasRecvChannel a where
--   getRecvChannel :: a -> TQueue ServerAudioMessage

-- class HasSendChannel a where
--   getSendChannel :: a -> TQueue ClientAudioMessage

instance (MonadAudioClient m) => MonadAudioClient (MaybeT m) where
  clientRecvChannel = lift clientRecvChannel
  clientSendChannel = lift clientSendChannel
  clientRecvMessage = lift clientRecvMessage
  clientSendMessage = lift . clientSendMessage
