{-# LANGUAGE DerivingVia #-}

module Mviz.Types (MvizError (..), MvizState (..), MvizM (..), runMviz) where

import Control.Exception qualified as E
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
-- import Control.Monad.Logger (MonadLogger (..), MonadLoggerIO (..))
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Mviz.Graphics.Shader qualified as Shader
import Mviz.SDL qualified
import Mviz.UI qualified

data MvizError
  = IOError E.IOException

data MvizState = MvizState
  { mvizWindow :: !Mviz.SDL.Window
  , mvizUiContext :: !Mviz.UI.UIContext
  , mvizShaders :: Map.Map T.Text Shader.ProgramObject
  }

-- newtype MvizM a = MvizM (StateT MvizState IO a)
--   deriving (Functor, Applicative, Monad)

newtype MvizM a = MvizM (StateT MvizState (ExceptT MvizError IO) a)
  deriving (Applicative,
             Functor,
             Monad,
             MonadIO,
             MonadError MvizError,
             MonadState MvizState)

runMviz :: MvizState -> MvizM a -> IO (Either MvizError (a, MvizState))
runMviz env (MvizM action) = runExceptT $ runStateT action env
