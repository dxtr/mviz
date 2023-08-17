{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

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
  , module ImGui.Raw.Structs
  , module ImGui.Raw.Types) where

import           Data.Text (Text)
import           Data.Text.Foreign (withCString)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Utils (with)

import ImGui.Raw.Structs
import ImGui.Raw.Types

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include "cimgui.h"
#include "cimgui_impl.h"
#include <float.h>

{#import ImGui.Raw.Structs #}
{#import ImGui.Raw.Types #}

-- System
{#fun unsafe igGetVersion as getVersion {} -> `String' #}
{#fun unsafe igGetDrawData as getDrawData {} -> `DrawData' id #}

--- Context
{#fun unsafe igCreateContext as createContext {id `FontAtlas'} -> `Context' id #}
{#fun unsafe igDestroyContext as destroyContext {id `Context'} -> `()' #}
{#fun unsafe igGetCurrentContext as getCurrentContext {} -> `Context' id #}

--- Frame
{#fun unsafe igNewFrame as newFrame {} -> `()' #}
{#fun unsafe igEndFrame as endFrame {} -> `()' #}

--- Window
tWithCString = Data.Text.Foreign.withCString
outmarshalBool :: Ptr CUChar -> IO Bool
outmarshalBool ptr = toBool <$> peek ptr

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
