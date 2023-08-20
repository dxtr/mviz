{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.SDL ( sdlNewFrame
                 , sdlShutdown
                 , sdlPollEvent
                 , sdlPollEvents
                ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign                (Ptr, alloca, castPtr)
import qualified Language.C.Inline      as C
import qualified Language.C.Inline.Cpp  as Cpp
import qualified SDL
import qualified SDL.Raw.Enum
import qualified SDL.Raw.Event

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdl2.h"
C.include "SDL.h"
Cpp.using "namespace ImGui";

sdlNewFrame :: MonadIO m => m ()
sdlNewFrame = liftIO $ do
  [C.exp| void { ImGui_ImplSDL2_NewFrame(); } |]

sdlShutdown :: MonadIO m => m ()
sdlShutdown = liftIO $ do
  [C.exp| void { ImGui_ImplSDL2_Shutdown(); } |]

sdlPollEvent :: MonadIO m => m (Maybe SDL.Event)
sdlPollEvent = liftIO $ do
  alloca $ \eventPtr -> do
    SDL.pumpEvents
    numEvents <- SDL.Raw.Event.peepEvents eventPtr 1 SDL.Raw.Enum.SDL_PEEKEVENT SDL.Raw.Enum.SDL_FIRSTEVENT SDL.Raw.Enum.SDL_LASTEVENT

    when (numEvents > 0) $ do
      let eventPtr' = castPtr eventPtr :: Ptr ()
      [C.exp| void { ImGui_ImplSDL2_ProcessEvent((SDL_Event*) $(void* eventPtr')) } |]

    SDL.pollEvent

sdlPollEvents :: MonadIO m => m [SDL.Event]
sdlPollEvents = liftIO $ do
  e <- sdlPollEvent
  case e of
    Nothing -> pure []
    Just e' -> (e':) <$> sdlPollEvents
