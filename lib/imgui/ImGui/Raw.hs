{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Raw
  ( Context
  , fltMin
  , getVersion
  , getDrawData
  , createContext
  , destroyContext
  , getCurrentContext
  , begin
  , beginCloseable
  , end
  , button
  , smallButton
  , selectable
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
  , textUnformatted
  , beginTooltip
  , beginItemTooltip
  , endTooltip
  , beginChild
  , endChild
  , lastItemData
  , isItemHovered
  , treeNode
  , treePop
  , collapsingHeader
  , checkbox
  , contentRegionAvail
  , calcTextSize
  , itemSpacing
  , itemInnerSpacing
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import           Data.Text.Foreign      (withCString)
import           Foreign                (Ptr, Storable (peek), alloca, fromBool,
                                         with)
import           Foreign.C              (CString)
import           Foreign.C.Types        (CBool (..), CFloat (..), CInt (CInt))
import           ImGui.Enums            (SelectableFlag, TreeNodeFlag,
                                         WindowFlag, combineFlags)
import           ImGui.Raw.Context      as CTX (imguiContext)
import           ImGui.Raw.Structs      (ImGuiContext, ImGuiLastItemData,
                                         ImVec2 (..))
import           ImGui.Types            (DrawData (..))
import qualified Language.C.Inline      as C
import qualified Language.C.Inline.Cpp  as Cpp

C.context (C.baseCtx <> Cpp.cppCtx <> C.bsCtx <> CTX.imguiContext)
C.include "imgui.h"
C.include "imgui_internal.h"
Cpp.using "namespace ImGui"

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

showDemoWindow :: (MonadIO m) => m Bool
showDemoWindow = liftIO $
  alloca $ \boolPtr -> do
    [C.exp| void { ShowDemoWindow($(bool* boolPtr)); } |]
    (0 /=) <$> peek boolPtr

showMetricsWindow :: (MonadIO m) => m Bool
showMetricsWindow = liftIO $
  alloca $ \boolPtr -> do
    [C.exp| void { ShowMetricsWindow($(bool* boolPtr)); } |]
    (0 /=) <$> peek boolPtr

showAboutWindow :: (MonadIO m) => m Bool
showAboutWindow = liftIO $
  alloca $ \boolPtr -> do
    [C.exp| void { ShowAboutWindow($(bool* boolPtr)); } |]
    (0 /=) <$> peek boolPtr

showUserGuide :: (MonadIO m) => m ()
showUserGuide = liftIO [C.exp| void { ShowUserGuide() } |]

fltMin :: (MonadIO m) => m CFloat
fltMin = liftIO [C.exp| float { -FLT_MIN } |]

--- Style
styleColorsDark :: (MonadIO m) => m ()
styleColorsDark = liftIO [C.exp| void { StyleColorsDark(); } |]

styleColorsLight :: (MonadIO m) => m ()
styleColorsLight = liftIO [C.exp| void { StyleColorsLight(); } |]

contentRegionAvail :: (MonadIO m) => m ImVec2
contentRegionAvail = liftIO $
  alloca $ \szPtr -> do
    [C.exp| void { *$(ImVec2* szPtr) = GetContentRegionAvail(); } |]
    peek szPtr

calcTextSize :: (MonadIO m) => T.Text -> Bool -> m ImVec2
calcTextSize text hideText = liftIO $
  alloca $ \szPtr ->
    withCString text $ \textPtr -> do
      [C.exp| void { *$(ImVec2* szPtr) = CalcTextSize($(char* textPtr), NULL, $(bool cHideText)); } |]
      peek szPtr
  where cHideText = fromBool hideText :: CBool

itemSpacing :: (MonadIO m) => m ImVec2
itemSpacing = liftIO $
  peek =<< [C.block|
    ImVec2* {
      ImGuiContext& g = *GImGui;
      ImGuiStyle& style = g.Style;
      return &style.ItemSpacing;
    }
  |]

itemInnerSpacing :: (MonadIO m) => m ImVec2
itemInnerSpacing = liftIO $
  peek =<< [C.block|
    ImVec2* {
      ImGuiContext& g = *GImGui;
      ImGuiStyle& style = g.Style;
      return &style.ItemInnerSpacing;
    }
  |]


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
newFrame = liftIO [C.exp| void { NewFrame(); } |]

endFrame :: (MonadIO m) => m ()
endFrame = liftIO [C.exp| void { EndFrame(); } |]

--- Window
beginCloseable :: (MonadIO m) => T.Text -> [WindowFlag] -> m (Bool, Bool)
beginCloseable name flags = liftIO $ do
  withCString name $ \namePtr ->
    with (1 :: CBool) $ \pOpen -> do
    collapsed <- (0 /=) <$> [C.exp| bool { Begin($(char* namePtr), $(bool* pOpen), $(ImGuiWindowFlags flags')) } |]
    pOpen' <- (0 /=) <$> peek pOpen
    pure (collapsed, pOpen')
  where flags' = combineFlags flags

begin :: (MonadIO m) => T.Text -> [WindowFlag] -> m Bool
begin label flags = liftIO $ do
  withCString label $ \labelPtr ->
    (0 /=) <$> [C.exp| bool { Begin($(char* labelPtr), nullptr, $(ImGuiWindowFlags flags')) } |]
  where flags' = combineFlags flags

end :: (MonadIO m) => m ()
end = liftIO $ do
  [C.exp| void { End(); } |]

beginChild :: (MonadIO m) => T.Text -> ImVec2 -> Bool -> [WindowFlag] -> m Bool
beginChild label size border flags = liftIO $ do
  withCString label $ \labelPtr ->
    with size $ \sizePtr ->
                  (0 /=) <$> [C.exp| bool { BeginChild($(char* labelPtr), *$(ImVec2* sizePtr), $(bool border'), $(ImGuiWindowFlags flags')) } |]
  where flags' = combineFlags flags
        border' = fromBool border

endChild :: (MonadIO m) => m ()
endChild = liftIO [C.exp| void { EndChild() } |]

isItemHovered :: (MonadIO m) => m Bool
isItemHovered = liftIO $ (0 /=) <$> [C.exp| bool { IsItemHovered() } |]

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
button :: (MonadIO m) => T.Text -> m Bool
button label = liftIO $ do
  withCString label $ \labelPtr ->
    (0 /=) <$> [C.exp| bool { Button($(char* labelPtr)) } |]

smallButton :: (MonadIO m) => CString -> m Bool
smallButton labelPtr = liftIO $ do
  (0 /=) <$> [C.exp| bool { SmallButton($(char* labelPtr)) } |]

--- Selectable
selectable :: (MonadIO m) => T.Text -> Bool -> [SelectableFlag] -> m Bool
selectable label selected flags = liftIO $ do
  withCString label $ \labelPtr ->
    (0 /=) <$> [C.exp| bool { Selectable($(char* labelPtr), $(bool selected'), $(ImGuiSelectableFlags flags')) } |]
  where flags' = combineFlags flags
        selected' = fromBool selected

--- Listbox
beginListBox :: (MonadIO m) => CString -> ImVec2 -> m Bool
beginListBox label size = liftIO $ do
  with size $ \sizePtr ->
    (0 /=) <$> [C.exp| bool { BeginListBox($(char* label), *$(ImVec2* sizePtr)) } |]

endListBox :: (MonadIO m) => m ()
endListBox = liftIO [C.exp| void { EndListBox(); } |]

-- Text
textUnformatted :: (MonadIO m) => CString -> m ()
textUnformatted text = liftIO [C.exp| void { TextUnformatted($(char* text)) } |]

-- Tooltip
beginTooltip :: (MonadIO m) => m Bool
beginTooltip = liftIO $ (0 /=) <$> [C.exp| bool { BeginTooltip() } |]

beginItemTooltip :: (MonadIO m) => m Bool
beginItemTooltip = liftIO $ (0 /=) <$> [C.exp| bool { BeginItemTooltip() } |]

-- TODO: BeginTooltipEx

endTooltip :: (MonadIO m) => m ()
endTooltip = liftIO [C.exp| void { EndTooltip() } |]

lastItemData :: (MonadIO m) => m ImGuiLastItemData
lastItemData = liftIO $ do
  res <- [C.exp| ImGuiLastItemData* { &(GImGui->LastItemData) } |]
  peek res

-- Trees
treeNode :: (MonadIO m) => CString -> m Bool
treeNode label = liftIO $ (0 /=) <$> [C.exp| bool { TreeNode($(char* label)) } |]

treePop :: MonadIO m => m ()
treePop = liftIO [C.exp| void { TreePop() } |]

-- Collapsing header
collapsingHeader :: (MonadIO m) => CString -> [TreeNodeFlag] -> m Bool
collapsingHeader label flags = liftIO $ (0 /=) <$> [C.exp| bool { CollapsingHeader($(char* label), $(ImGuiTreeNodeFlags flags')) } |]
  where flags' = combineFlags flags

-- Checkbox
checkbox :: (MonadIO m) => CString -> Ptr CBool -> m Bool
checkbox labelPtr boolPtr = liftIO $ do
  (0 /=) <$> [C.exp| bool { Checkbox($(char* labelPtr), $(bool* boolPtr)) } |]
