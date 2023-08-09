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
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Mviz.Audio (AudioMessage (..), AudioReturn)
import Mviz.Graphics.Shader qualified as Shader
import Mviz.SDL qualified
import Mviz.UI qualified

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
  }

newtype MvizM a
  = MvizM (ReaderT MvizEnvironment (StateT MvizState (ExceptT MvizError IO)) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadError MvizError
    , MonadState MvizState
    , MonadReader MvizEnvironment
    )

runMviz
  :: MvizEnvironment
  -> MvizM a
  -> IO (Either MvizError a)
runMviz environment (MvizM action) = runExceptT runState
 where
  initialState = MvizState{mvizShaders = Map.empty, mvizShowUI = True}
  runReader = runReaderT action environment
  runState = evalStateT runReader initialState
