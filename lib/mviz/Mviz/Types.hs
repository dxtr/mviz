module Mviz.Types (
  MvizError (..),
  MvizState (..),
  MvizEnvironment (..),
  MvizFramerate (..),
  MvizM (..),
  runMviz,
) where

import           Control.Concurrent.Async   (Async)
import           Control.Concurrent.STM     (TChan)
import qualified Control.Exception          as E
import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (LoggingT, MonadLogger)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Data.Word                  (Word64)
import           Mviz.Audio                 (AudioMessage (..), AudioReturn)
import qualified Mviz.Graphics.Shader       as Shader
import           Mviz.Logger                (LogMessage, runRingbufferLoggingT)
import qualified Mviz.SDL
import qualified Mviz.UI.Types              as UITypes
import           Mviz.UI.UIWindow           (LogWindow, makeLogWindow)
import qualified Mviz.Utils.Ringbuffer      as RB

data MvizError
  = IOError E.IOException
  | NoError

data MvizEnvironment = MvizEnvironment
  { mvizWindow           :: Mviz.SDL.Window
  , mvizUIContext        :: UITypes.UIContext
  , mvizAudioThread      :: Async AudioReturn
  , mvizAudioSendChannel :: TChan AudioMessage
  , mvizAudioRecvChannel :: TChan AudioMessage
  }

data MvizFramerate = MvizFramerate
  { mvizFramerate        :: Word -- The calculated framerate
  , mvizFramerateSample  :: Word64 -- The last time (In ticks) the framerate was sampled
  , mvizFramerateCounter :: Word -- The amount of frames drawn since the last sample was taken
  }

data MvizState = MvizState
  { mvizShaders   :: Map.Map T.Text Shader.ProgramObject
  , mvizShowUI    :: Bool
  , mvizShowLogWindow :: Bool
  , mvizLogWindow :: LogWindow

--  , mvizShowDebugLog :: Bool
--  , mvizShowConsole :: Bool
  , mvizLog       :: RB.Ringbuffer LogMessage
  , mvizFPS       :: MvizFramerate
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
  logWindow <- makeLogWindow True
  let initialState =
        MvizState
          { mvizShaders = Map.empty
          , mvizShowUI = True
          , mvizLogWindow = logWindow
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
