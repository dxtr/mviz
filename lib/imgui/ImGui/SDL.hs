{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}

module ImGui.SDL
  ( sdlShutdown
  , sdlNewFrame
  , sdlPollEvent
  , sdlPollEvents
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Foreign.C.Types
import           Foreign.Ptr            (Ptr, castPtr)
import           ImGui.Raw.SDL          as Raw
import qualified Language.C.Inline      as C
import qualified SDL
import           SDL.Event              (Event)
import qualified SDL.Raw

C.context (C.bsCtx)
C.include "SDL.h"
C.include "cimgui.h"
C.include "cmigui_impl.h"

sdlShutdown :: MonadIO m => m ()
sdlShutdown = liftIO $ Raw.sdlShutdown

sdlNewFrame :: MonadIO m => m ()
sdlNewFrame = liftIO $ Raw.sdlNewFrame

sdlPollEvent :: MonadIO m => m (Maybe Event)
sdlPollEvent = liftIO do
  alloca \eventPtr -> do
    SDL.pumpEvents
    numEvents <- SDL.Raw.peepEvents eventPtr 1 SDL.Raw.SDL_PEEKEVENT SDL.Raw.SDL_FIRSTEVENT SDL.Raw.SDL_LASTEVENT
    when (numEvents > 0) do
      let eventPtr' = castPtr eventPtr :: Ptr ()
      [C.exp| void{ ImGui_ImplSDL2_ProcessEvent((SDL_Event*) $(void* eventPtr')) } |]

    SDL.pollEvent

sdlPollEvents :: MonadIO m => m [Event]
sdlPollEvents = do
  evt <- pollEvent
  case e of
    Nothing -> return []
    Just e' -> (e':) <$> pollevents
