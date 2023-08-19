{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

module ImGui.Raw.GL
  ( glInit
  , glShutdown
  , glNewFrame
  , glRenderDrawData
  ) where

import Data.Text (Text)
import Foreign
import Foreign.C
import ImGui.Raw.Utils

#include "cimgui.h"
#include "cimgui_impl.h"

{#import ImGui.Raw.Types #}

-- CIMGUI_API bool ImGui_ImplOpenGL3_CreateFontsTexture(void);
-- CIMGUI_API void ImGui_ImplOpenGL3_DestroyFontsTexture(void);
-- CIMGUI_API bool ImGui_ImplOpenGL3_CreateDeviceObjects(void);
-- CIMGUI_API void ImGui_ImplOpenGL3_DestroyDeviceObjects(void);

{#fun unsafe ImGui_ImplOpenGL3_Init as glInit {tWithCString* `Text'} -> `Bool' #}
{#fun unsafe ImGui_ImplOpenGL3_Shutdown as glShutdown {} -> `()' #}
{#fun unsafe ImGui_ImplOpenGL3_NewFrame as glNewFrame {} -> `()' #}
{#fun unsafe ImGui_ImplOpenGL3_RenderDrawData as glRenderDrawData {id `DrawData'} -> `()' #}

