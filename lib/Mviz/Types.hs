module Mviz.Types (
  MvizError (..),
  MvizState (..),
  MvizEnvironment (..),
  MvizFramerate (..),
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
import Data.Word (Word64)
import Mviz.Audio (AudioMessage (..), AudioReturn)
import Mviz.Graphics.Shader qualified as Shader
import Mviz.Logger (LogMessage, runRingbufferLoggingT)
import Mviz.SDL qualified
import Mviz.UI.Types qualified as UITypes
import Mviz.Utils.Ringbuffer qualified as RB

data MvizError
  = IOError E.IOException
  | NoError

data MvizEnvironment = MvizEnvironment
  { mvizWindow :: Mviz.SDL.Window
  , mvizUIContext :: UITypes.UIContext
  , mvizAudioThread :: Async AudioReturn
  , mvizAudioSendChannel :: TChan AudioMessage
  , mvizAudioRecvChannel :: TChan AudioMessage
  }

data MvizFramerate = MvizFramerate
  { mvizFramerate :: Word -- The calculated framerate
  , mvizFramerateSample :: Word64 -- The last time (In ticks) the framerate was sampled
  , mvizFramerateCounter :: Word -- The amount of frames drawn since the last sample was taken
  }

data MvizState = MvizState
  { mvizShaders :: Map.Map T.Text Shader.ProgramObject
  , mvizShowUI :: Bool
  , mvizLog :: RB.Ringbuffer LogMessage
  , mvizFPS :: MvizFramerate
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
  logBuffer <- RB.empty 100
  let initialState =
        MvizState
          { mvizShaders = Map.empty
          , mvizShowUI = True
          , mvizLog = logBuffer
          , mvizFPS =
              MvizFramerate
                { mvizFramerate = 0
                , mvizFramerateSample = 0
                , mvizFramerateCounter = 0
                }
          }
  runExceptT $ runLogger initialState
 where
  runReader = runReaderT action environment
  runState initialState = evalStateT runReader initialState
  runLogger initialState = runRingbufferLoggingT (mvizLog initialState) $ runState initialState
