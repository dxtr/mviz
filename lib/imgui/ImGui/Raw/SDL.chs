{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Raw.SDL
  ( sdlShutdown
  , sdlNewFrame
  ) where

import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (alloca)

{#import ImGui.Raw.Types #}

#include "cimgui.h"
#include "cimgui_impl.h"

-- typedef struct SDL_Window SDL_Window;
-- typedef struct SDL_Renderer SDL_Renderer;
-- struct SDL_Window;
-- struct SDL_Renderer;
-- typedef union SDL_Event SDL_Event;
-- CIMGUI_API bool ImGui_ImplSDL2_InitForOpenGL(SDL_Window* window,void* sdl_gl_context);
-- CIMGUI_API void ImGui_ImplSDL2_Shutdown(void);
-- CIMGUI_API void ImGui_ImplSDL2_NewFrame(void);
-- CIMGUI_API bool ImGui_ImplSDL2_ProcessEvent(const SDL_Event* event);

{#fun unsafe ImGui_ImplSDL2_Shutdown as sdlShutdown {} -> `()' #}
{#fun unsafe ImGui_ImplSDL2_NewFrame as sdlNewFrame {} -> `()' #}
