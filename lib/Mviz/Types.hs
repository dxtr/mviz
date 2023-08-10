module Mviz.Types (
  MvizError (..),
  MvizState (..),
  MvizEnvironment (..),
  MvizM (..),
  runMviz,
) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TChan)
import Control.Exception qualified as E
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Mviz.Audio (AudioMessage (..), AudioReturn)
import Mviz.Graphics.Shader qualified as Shader
import Mviz.Logger (LogMessage, runRingbufferLoggingT)
import Mviz.SDL qualified
import Mviz.UI qualified
import Mviz.Utils.Ringbuffer qualified as RB

data MvizError
  = IOError E.IOException
  | NoError

data MvizEnvironment = MvizEnvironment
  { mvizWindow :: Mviz.SDL.Window
  , mvizUIContext :: Mviz.UI.UIContext
  , mvizAudioThread :: Async AudioReturn
  , mvizAudioSendChannel :: TChan AudioMessage
  , mvizAudioRecvChannel :: TChan AudioMessage
  }

data MvizState = MvizState
  { mvizShaders :: Map.Map T.Text Shader.ProgramObject
  , mvizShowUI :: Bool
  , mvizLog :: RB.Ringbuffer LogMessage
  }

newtype MvizM a
  = MvizM
      (ReaderT MvizEnvironment (StateT MvizState (LoggingT (ExceptT MvizError IO))) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadError MvizError
    , MonadState MvizState
    , MonadReader MvizEnvironment
    , MonadLogger
    )

runMviz
  :: MvizEnvironment
  -> MvizM a
  -> IO (Either MvizError a)
runMviz environment (MvizM action) = do
  lgr <- RB.empty 100
  let initialState =
        MvizState
          { mvizShaders = Map.empty
          , mvizShowUI = True
          , mvizLog = lgr
          }
  runExceptT $ runLogger initialState
 where
  runReader = runReaderT action environment
  runState initialState = evalStateT runReader initialState
  runLogger initialState = runRingbufferLoggingT (mvizLog initialState) $ runState initialState
