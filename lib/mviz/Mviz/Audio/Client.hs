module Mviz.Audio.Client (createClient) where

import qualified Control.Monad.Exception.Synchronous as Sync
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Mviz.Audio.Types                    (AudioError (..))
import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Exception                as JACKE

type JackReturnType a = Sync.ExceptionalT JACKE.All IO a

mapJackException :: JACKE.All -> AudioError
mapJackException ex = JACKError $ JACKE.toStringWithHead ex

jackAction :: (MonadIO m) => JackReturnType a -> m (Either AudioError a)
jackAction action = liftIO . Sync.toEitherT . Sync.mapExceptionT mapJackException $ action

createClient :: (MonadIO m) => String -> m (Either AudioError JACK.Client)
createClient name = jackAction $ JACK.newClientDefault name

--  runExceptT $ do
--  Sync.toEitherT . Sync.mapExceptionT mapJackException $ JACK.newClientDefault name
