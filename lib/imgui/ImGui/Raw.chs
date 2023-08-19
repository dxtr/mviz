{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Raw
  ( getVersion
  , getDrawData
  , createContext
  , destroyContext
  , getCurrentContext
  , begin
  , end
  , button
  , selectable
  , beginListBox
  , endListBox
  , beginGroup
  , endGroup
  , newFrame
  , endFrame
  , newline
  , spacing
  , separator
  , render
  , showUserGuide
  , showDemoWindow
  , showMetricsWindow
  , styleColorsDark
  , checkVersion
  , module ImGui.Raw.Structs
  , module ImGui.Raw.Types) where

import           Data.Text (Text)
import           Foreign
import           Foreign.C.Types
import qualified Language.C.Inline as C
import Control.Monad.IO.Class (MonadIO, liftIO)

import ImGui.Raw.Utils

#include "cimgui.h"

{#import ImGui.Raw.Structs #}
{#import ImGui.Raw.Types #}

-- System
{#fun unsafe igGetVersion as getVersion {} -> `String' #}
{#fun unsafe igGetDrawData as getDrawData {} -> `DrawData' id #}
{#fun unsafe igRender as render {} -> `()' #}
{#fun unsafe igShowUserGuide as showUserGuide {} -> `()' #}
{#fun unsafe igShowDemoWindow as showDemoWindow {alloca- `Bool' outmarshalBool*} -> `()' #}
{#fun unsafe igShowMetricsWindow as showMetricsWindow {alloca- `Bool' outmarshalBool*} -> `()' #}

checkVersion :: (MonadIO m) => m ()
checkVersion = liftIO $ [C.exp| void { IMGUI_CHECKVERSION(); } |]

--- Style
{#fun unsafe igStyleColorsDark as styleColorsDark {id `Ptr Style'} -> `()' #}

--- Context
{#fun unsafe igCreateContext as createContext {id `Ptr FontAtlas'} -> `Context' id #}
{#fun unsafe igDestroyContext as destroyContext {id `Context'} -> `()' #}
{#fun unsafe igGetCurrentContext as getCurrentContext {} -> `Context' id #}

--- Frame
{#fun unsafe igNewFrame as newFrame {} -> `()' #}
{#fun unsafe igEndFrame as endFrame {} -> `()' #}

--- Window
{#fun unsafe igBegin as begin
  { tWithCString* `Text'
  , alloca- `Bool' outmarshalBool*
  , combineWindowFlags `[WindowFlag]'
  } -> `()' #}
{#fun unsafe igEnd as end {} -> `()' #}

-- Widgets
--- Misc
{#fun unsafe igSeparator as separator {} -> `()' #}
{#fun unsafe igNewLine as newline {} -> `()' #}
{#fun unsafe igSpacing as spacing {} -> `()' #}
{#fun unsafe igBeginGroup as beginGroup {} -> `()' #}
{#fun unsafe igEndGroup as endGroup {} -> `()' #}

--- Button
{#fun unsafe igButton as button { tWithCString* `Text', with* %`Vec2' } -> `Bool' #}

--- Selectable
{#fun unsafe igSelectable_Bool as selectable
  { tWithCString* `Text'
  , `Bool'
  , combineSelectableFlags `[SelectableFlag]'
  , with* %`Vec2'
  } -> `Bool' #}

--- Listbox
{#fun igBeginListBox as beginListBox { tWithCString* `Text', with* %`Vec2'} -> `Bool' #}

{#fun unsafe igEndListBox as endListBox {} -> `()' #}
