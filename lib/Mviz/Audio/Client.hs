module Mviz.Audio.Client (createClient) where

import Control.Monad.Exception.Synchronous qualified as Sync
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mviz.Audio.Types (AudioError (..))
import Sound.JACK qualified as JACK
import Sound.JACK.Exception qualified as JACKE

type JackReturnType a = Sync.ExceptionalT JACKE.All IO a

mapJackException :: JACKE.All -> AudioError
mapJackException ex = JACKError $ JACKE.toStringWithHead ex

jackAction :: (MonadIO m) => JackReturnType a -> m (Either AudioError a)
jackAction action = liftIO . Sync.toEitherT . Sync.mapExceptionT mapJackException $ action

createClient :: (MonadIO m) => String -> m (Either AudioError JACK.Client)
createClient name = jackAction $ JACK.newClientDefault name

--  runExceptT $ do
--  Sync.toEitherT . Sync.mapExceptionT mapJackException $ JACK.newClientDefault name
