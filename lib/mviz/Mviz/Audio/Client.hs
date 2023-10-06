module Mviz.Audio.Client
  ( createClient
  , activateClient
  , deactivateClient
  , jackAction
  , getPorts
  , getBufferSize
  , getSampleRate
  ) where

import           Control.Exception                   (throwIO)
import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Functor                        ((<&>))
import qualified Data.Text                           as T
import           Mviz.Audio.Types                    (AudioError (..),
                                                      JackReturnType)
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Exception                as JACKE

mapJackException :: JACKE.All -> AudioError
mapJackException ex = JACKError $ JACKE.toStringWithHead ex

-- jackAction :: (MonadIO m) => JackReturnType a -> m (Either AudioError a)
-- jackAction action = liftIO . Sync.toEitherT . Sync.mapExceptionT mapJackException $ action
jackAction :: (MonadIO m) => JackReturnType a -> m a
jackAction action = do
  actionRes <- liftIO . Sync.toEitherT . Sync.mapExceptionT mapJackException $ action
  case actionRes of
    Right r -> return r
    Left e  -> liftIO $ throwIO e

createClient :: (MonadIO m) => String -> m JACK.Client
createClient = jackAction . JACK.newClientDefault

activateClient :: (MonadIO m) => JACK.Client -> m ()
activateClient = jackAction . JACK.activate

deactivateClient :: (MonadIO m) => JACK.Client -> m ()
deactivateClient = jackAction . JACK.deactivate

-- closeClient :: (MonadIO m) => JACK.Client -> m ()
-- closeClient = jackAction . JACK.clientClose

getPorts :: (MonadIO m) => JACK.Client -> m [T.Text]
getPorts client = liftIO (JACK.getPorts client <&> fmap T.pack)

getBufferSize :: (MonadIO m) => JACK.Client -> m Int
getBufferSize = liftIO . JACK.getBufferSize

getSampleRate :: (MonadIO m) => JACK.Client -> m Int
getSampleRate = liftIO . JACK.getSampleRate
