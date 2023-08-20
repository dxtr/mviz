{-# LANGUAGE QuasiQuotes     #-}
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
  , smallButton
  , selectable
  , listBox
  , beginListBox
  , endListBox
  , beginGroup
  , endGroup
  , newFrame
  , endFrame
  , newLine
  , sameLine
  , spacing
  , separator
  , render
  , showAboutWindow
  , showUserGuide
  , showDemoWindow
  , showMetricsWindow
  , styleColorsDark
  , styleColorsLight
  , checkVersion
  , setCurrentContext
  , dummy
  , indent
  , unindent
  , module ImGui.Types
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign
import           Foreign.C              (CString)
import           Foreign.C.Types
import           ImGui.Enums
import           ImGui.Raw.Context
import           ImGui.Structs
import           ImGui.Types
import qualified Language.C.Inline      as C
import qualified Language.C.Inline.Cpp  as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui";

newtype Context = Context (Ptr ImGuiContext)

-- System
checkVersion :: (MonadIO m) => m ()
checkVersion = liftIO [C.exp| void { IMGUI_CHECKVERSION(); } |]

getVersion :: MonadIO m => m CString
getVersion = liftIO [C.exp| const char* { GetVersion() } |]

getDrawData :: MonadIO m => m DrawData
getDrawData = liftIO $ do
  DrawData <$> [C.exp| void* { GetDrawData() }|]

render :: (MonadIO m) => m ()
render = liftIO [C.exp| void { Render(); } |]

showDemoWindow :: (MonadIO m) => m ()
showDemoWindow = liftIO [C.exp| void { ShowDemoWindow(); } |]

showMetricsWindow :: (MonadIO m) => m ()
showMetricsWindow = liftIO [C.exp| void { ShowMetricsWindow(); } |]

showAboutWindow :: (MonadIO m) => m ()
showAboutWindow = liftIO [C.exp| void { ShowAboutWindow(); } |]

showUserGuide :: (MonadIO m) => m ()
showUserGuide = liftIO [C.exp| void { ShowUserGuide() } |]

--- Style
styleColorsDark :: (MonadIO m) => m ()
styleColorsDark = liftIO [C.exp| void { StyleColorsDark(); } |]

styleColorsLight :: (MonadIO m) => m ()
styleColorsLight = liftIO [C.exp| void { StyleColorsLight(); } |]

--- Context
createContext :: (MonadIO m) => m Context
createContext = liftIO $ Context <$> [C.exp| ImGuiContext* { CreateContext() } |]

destroyContext :: (MonadIO m) => Context -> m ()
destroyContext (Context contextPtr) = liftIO [C.exp| void { DestroyContext($(ImGuiContext* contextPtr)); } |]

getCurrentContext :: MonadIO m => m Context
getCurrentContext = liftIO $ do
  Context <$> [C.exp| ImGuiContext* { GetCurrentContext() } |]

setCurrentContext :: MonadIO m => Context -> m ()
setCurrentContext (Context contextPtr) = liftIO $ do
  [C.exp| void { SetCurrentContext($(ImGuiContext* contextPtr)) } |]

--- Frame
newFrame :: (MonadIO m) => m ()
newFrame = liftIO $ do
  [C.exp| void { NewFrame(); } |]

endFrame :: (MonadIO m) => m ()
endFrame = liftIO $ do
  [C.exp| void { EndFrame(); } |]

--- Window
begin :: (MonadIO m) => CString -> [ImGuiWindowFlag] -> m Bool
begin namePtr flags = liftIO $ do
  (0 /=) <$> [C.exp| bool { Begin($(char* namePtr), nullptr, $(int flags')) } |]
  where flags' = windowFlagValue $ combineWindowFlags flags

end :: (MonadIO m) => m ()
end = liftIO $ do
  [C.exp| void { End(); } |]

-- Widgets
--- Misc
separator :: (MonadIO m) => m ()
separator = liftIO $ do
  [C.exp| void { Separator(); } |]

sameLine :: (MonadIO m) => m ()
sameLine = liftIO $ do
  [C.exp| void { SameLine(); } |]

newLine :: (MonadIO m) => m ()
newLine = liftIO $ do
  [C.exp| void { NewLine() } |]

spacing :: (MonadIO m) => m ()
spacing = liftIO $ do
  [C.exp| void { Spacing() } |]

dummy :: (MonadIO m) => Ptr ImVec2 -> m ()
dummy sizePtr = liftIO $ do
  [C.exp| void { Dummy(*$(ImVec2* sizePtr)) } |]

indent :: (MonadIO m) => CFloat -> m ()
indent indent_w = liftIO $ do
  [C.exp| void { Indent($(float indent_w)) } |]

unindent :: (MonadIO m) => CFloat -> m ()
unindent indent_w = liftIO $ do
  [C.exp| void { Unindent($(float indent_w)) } |]

beginGroup :: (MonadIO m) => m ()
beginGroup = liftIO $ do
  [C.exp| void { BeginGroup() } |]

endGroup :: (MonadIO m) => m ()
endGroup = liftIO $ do
  [C.exp| void { EndGroup() } |]

--- Button
button :: (MonadIO m) => CString -> m Bool
button labelPtr = liftIO $ do
  (0 /=) <$> [C.exp| bool { Button($(char* labelPtr)) } |]

smallButton :: (MonadIO m) => CString -> m Bool
smallButton labelPtr = liftIO $ do
  (0 /=) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]

--- Selectable
selectable :: (MonadIO m) => CString -> CBool -> [ImGuiSelectableFlag] -> Ptr ImVec2 -> m Bool
selectable labelPtr selected flags size = liftIO $ do
  (0 /=) <$> [C.exp| bool { Selectable($(char* labelPtr), $(bool selected), $(ImGuiSelectableFlags flags'), *$(ImVec2 *size)) } |]
  where flags' = combineSelectableFlags flags

--- Listbox
listBox :: (MonadIO m) => CString -> Ptr CInt -> Ptr CString -> CInt -> m Bool
listBox labelPtr iPtr itemsPtr itemsLen = liftIO $ do
  (0 /=) <$> [C.exp| bool { ListBox($(char* labelPtr), $(int* iPtr), $(char** itemsPtr), $(int itemsLen)) }|]

beginListBox :: (MonadIO m) => CString -> Ptr ImVec2 -> m Bool
beginListBox label size = liftIO $ do
  (0 /=) <$> [C.exp| bool { BeginListBox($(char* label), *$(ImVec2* size)) } |]

endListBox :: (MonadIO m) => m ()
endListBox = liftIO [C.exp| void { EndListBox(); } |]

-- {#fun igBeginListBox as beginListBox { tWithCString* `Text', with* %`Vec2'} -> `Bool' #}
-- {#fun unsafe igEndListBox as endListBox {} -> `()' #}
