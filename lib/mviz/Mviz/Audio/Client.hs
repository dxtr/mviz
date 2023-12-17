module Mviz.Audio.Client
  ( createClient
  , activateClient
  , deactivateClient
  , jackAction
  , getPorts
  , getBufferSize
  , getSampleRate
  , withClient
  , withActivation
  ) where

import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Functor                        ((<&>))
import qualified Data.Text                           as T
import           GHC.Stack                           (HasCallStack)
import           Mviz.Audio.Types                    (AudioError (..),
                                                      AudioException (AudioException),
                                                      JackReturnType)
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Exception                as JACKE
import           UnliftIO                            (MonadUnliftIO, bracket,
                                                      bracket_)
import           UnliftIO.Exception                  (throwIO)

mapJackException :: JACKE.All -> AudioError
mapJackException (JACKE.Status statusSet) =
  AudioError { audioErrorMessage = "Status " <> JACKE.toString statusSet }
mapJackException (JACKE.NoStatus (JACKE.NoPortRegister (JACKE.PortMismatch JACKE.TypeMismatch))) =
  AudioError { audioErrorMessage = "NoStatus,NoPortRegister,PortMismatch,TypeMismatch" }
mapJackException (JACKE.NoStatus (JACKE.NoPortRegister (JACKE.PortMismatch JACKE.DirectionMismatch))) =
  AudioError { audioErrorMessage = "NoStatus,NoPortRegister,PortMismatch,DirectionMismatch" }
mapJackException (JACKE.NoStatus JACKE.PortRegister) =
  AudioError { audioErrorMessage = "NoStatus,PortRegister" }
mapJackException (JACKE.NoStatus (JACKE.NoPortRegister (JACKE.NoPortMismatch e))) =
  AudioError { audioErrorMessage = "NoStatus,NoPortRegister,NoPortMismatch " <> JACKE.toString e }

jackAction :: (MonadIO m, HasCallStack) => JackReturnType a -> m a
jackAction action = do
  actionRes <- liftIO . Sync.toEitherT . Sync.mapExceptionT mapJackException $ action
  case actionRes of
    Right r -> pure r
    Left e  -> liftIO $ throwIO $ AudioException e

createClient :: (MonadIO m) => String -> m JACK.Client
createClient = jackAction . JACK.newClientDefault

destroyClient :: (MonadIO m) => JACK.Client -> m ()
destroyClient = jackAction . JACK.disposeClient

withClient :: (MonadUnliftIO m) => String -> (JACK.Client -> m ()) -> m ()
withClient clientName = bracket (createClient clientName) destroyClient

activateClient :: (MonadIO m) => JACK.Client -> m ()
activateClient = jackAction . JACK.activate

deactivateClient :: (MonadIO m) => JACK.Client -> m ()
deactivateClient = jackAction . JACK.deactivate

withActivation :: (MonadUnliftIO m) => JACK.Client -> m () -> m ()
withActivation client = bracket_ (activateClient client) (deactivateClient client)

-- closeClient :: (MonadIO m) => JACK.Client -> m ()
-- closeClient = jackAction . JACK.clientClose

getPorts :: (MonadIO m) => JACK.Client -> m [T.Text]
getPorts client = liftIO (JACK.getPorts client <&> fmap T.pack)

getBufferSize :: (MonadIO m) => JACK.Client -> m Word
getBufferSize client =
  liftIO (JACK.getBufferSize client <&> fromIntegral)

getSampleRate :: (MonadIO m) => JACK.Client -> m Word
getSampleRate client =
  liftIO (JACK.getSampleRate client <&> fromIntegral)
