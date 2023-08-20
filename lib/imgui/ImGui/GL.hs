{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.GL ( glInit
                , glShutdown
                , glNewFrame
                , glRenderDrawData
                ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           ImGui.Raw.Context
import           ImGui.Types            (DrawData (..))
import qualified Language.C.Inline      as C
import qualified Language.C.Inline.Cpp  as Cpp

C.context (Cpp.cppCtx <> imguiContext)
C.include "backends/imgui_impl_opengl3.h"
Cpp.using "namespace ImGui"

glInit :: MonadIO m => m Bool
glInit = liftIO $ (0 /=) <$> [C.exp| bool { ImGui_ImplOpenGL3_Init() } |]

glShutdown :: MonadIO m => m ()
glShutdown = liftIO [C.exp| void { ImGui_ImplOpenGL3_Shutdown(); } |]

glNewFrame :: MonadIO m => m ()
glNewFrame = liftIO [C.exp| void { ImGui_ImplOpenGL3_NewFrame(); } |]

glRenderDrawData :: MonadIO m => DrawData -> m ()
glRenderDrawData (DrawData ptr) = liftIO [C.exp| void { ImGui_ImplOpenGL3_RenderDrawData((ImDrawData*) $( void* ptr )) } |]
