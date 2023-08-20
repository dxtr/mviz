{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Enums
    ( ImGuiWindowFlag(..)
    , ImGuiSelectableFlag(..)
    , combineWindowFlags
    , combineSelectableFlags
    , imGuiWindowFlagNone
    , imGuiWindowFlagNoTitleBar
    , imGuiWindowFlagNoResize
    , imGuiWindowFlagNoMove
    , imGuiWindowFlagNoScrollbar
    , imGuiWindowFlagNoScrollWithMouse
    , imGuiWindowFlagNoCollapse
    , imGuiWindowFlagAlwaysAutoResize
    , imGuiWindowFlagNoBackground
    , imGuiWindowFlagNoSavedSettings
    , imGuiWindowFlagNoMouseInputs
    , imGuiWindowFlagMenuBar
    , imGuiWindowFlagHorizontalScrollbar
    , imGuiWindowFlagNoFocusOnAppearing
    , imGuiWindowFlagNoBringToFrontOnFocus
    , imGuiWindowFlagAlwaysVerticalScrollbar
    , imGuiWindowFlagAlwaysHorizontalScrollbar
    , imGuiWindowFlagAlwaysUseWindowPadding
    , imGuiWindowFlagNoNavInputs
    , imGuiWindowFlagNoNavFocus
    , imGuiWindowFlagUnsavedDocument
    , imGuiWindowFlagNoDocking
    , imGuiWindowFlagNoNav
    , imGuiWindowFlagNoDecoration
    , imGuiWindowFlagNoInputs
    , imGuiSelectableFlagNone
    , imGuiSelectableFlagDontClosePopups
    , imGuiSelectableFlagSpanAllColumns
    , imGuiSelectableFlagAllowDoubleClick
    , imGuiSelectableFlagDisabled) where
import           Data.Bits             (shiftL, (.|.))
import           Foreign.C             (CInt)
import qualified Language.C.Inline     as C
import qualified Language.C.Inline.Cpp as Cpp

C.context Cpp.cppCtx
C.include "imgui.h"
Cpp.using "namespace ImGui";

-- Window flags
newtype ImGuiWindowFlag = ImGuiWindowFlag { windowFlagValue :: CInt } deriving (Show, Eq)

combineWindowFlags :: [ImGuiWindowFlag] -> ImGuiWindowFlag
combineWindowFlags flags = ImGuiWindowFlag $ foldr (\val acc -> acc .|. windowFlagValue val) 0 flags

imGuiWindowFlagNone :: ImGuiWindowFlag
imGuiWindowFlagNone                   = ImGuiWindowFlag 0

imGuiWindowFlagNoTitleBar :: ImGuiWindowFlag
imGuiWindowFlagNoTitleBar             = ImGuiWindowFlag $ 1 `shiftL` 0

imGuiWindowFlagNoResize :: ImGuiWindowFlag
imGuiWindowFlagNoResize               = ImGuiWindowFlag $ 1 `shiftL` 1

imGuiWindowFlagNoMove :: ImGuiWindowFlag
imGuiWindowFlagNoMove                 = ImGuiWindowFlag $ 1 `shiftL` 2  --  Disable user moving the window

imGuiWindowFlagNoScrollbar :: ImGuiWindowFlag
imGuiWindowFlagNoScrollbar            = ImGuiWindowFlag $ 1 `shiftL` 3  --  Disable scrollbars (window can still scroll with mouse or programmatically)

imGuiWindowFlagNoScrollWithMouse :: ImGuiWindowFlag
imGuiWindowFlagNoScrollWithMouse      = ImGuiWindowFlag $ 1 `shiftL` 4  --  Disable user vertically scrolling with mouse wheel. On child window, mouse wheel will be forwarded to the parent unless NoScrollbar is also set.

imGuiWindowFlagNoCollapse :: ImGuiWindowFlag
imGuiWindowFlagNoCollapse             = ImGuiWindowFlag $ 1 `shiftL` 5  --  Disable user collapsing window by double-clicking on it. Also referred to as Window Menu Button (e.g. within a docking node).

imGuiWindowFlagAlwaysAutoResize :: ImGuiWindowFlag
imGuiWindowFlagAlwaysAutoResize       = ImGuiWindowFlag $ 1 `shiftL` 6  --  Resize every window to its content every frame

imGuiWindowFlagNoBackground :: ImGuiWindowFlag
imGuiWindowFlagNoBackground           = ImGuiWindowFlag $ 1 `shiftL` 7  --  Disable drawing background color (WindowBg, etc.) and outside border. Similar as using SetNextWindowBgAlpha(0.0f).

imGuiWindowFlagNoSavedSettings :: ImGuiWindowFlag
imGuiWindowFlagNoSavedSettings        = ImGuiWindowFlag $ 1 `shiftL` 8  --  Never load/save settings in .ini file

imGuiWindowFlagNoMouseInputs :: ImGuiWindowFlag
imGuiWindowFlagNoMouseInputs          = ImGuiWindowFlag $ 1 `shiftL` 9  --  Disable catching mouse, hovering test with pass through.

imGuiWindowFlagMenuBar :: ImGuiWindowFlag
imGuiWindowFlagMenuBar                = ImGuiWindowFlag $ 1 `shiftL` 10 --  Has a menu-bar

imGuiWindowFlagHorizontalScrollbar :: ImGuiWindowFlag
imGuiWindowFlagHorizontalScrollbar    = ImGuiWindowFlag $ 1 `shiftL` 11 --  Allow horizontal scrollbar to appear (off by default). You may use SetNextWindowContentSize(ImVec2(width,0.0f)); prior to calling Begin() to specify width. Read code in imgui_demo in the "Horizontal Scrolling" section.

imGuiWindowFlagNoFocusOnAppearing :: ImGuiWindowFlag
imGuiWindowFlagNoFocusOnAppearing     = ImGuiWindowFlag $ 1 `shiftL` 12 --  Disable taking focus when transitioning from hidden to visible state

imGuiWindowFlagNoBringToFrontOnFocus :: ImGuiWindowFlag
imGuiWindowFlagNoBringToFrontOnFocus  = ImGuiWindowFlag $ 1 `shiftL` 13 --  Disable bringing window to front when taking focus (e.g. clicking on it or programmatically giving it focus)

imGuiWindowFlagAlwaysVerticalScrollbar :: ImGuiWindowFlag
imGuiWindowFlagAlwaysVerticalScrollbar= ImGuiWindowFlag $ 1 `shiftL` 14 --  Always show vertical scrollbar (even if ContentSize.y < Size.y)

imGuiWindowFlagAlwaysHorizontalScrollbar :: ImGuiWindowFlag
imGuiWindowFlagAlwaysHorizontalScrollbar= ImGuiWindowFlag $ 1 `shiftL` 15 --  Always show horizontal scrollbar (even if ContentSize.x < Size.x)

imGuiWindowFlagAlwaysUseWindowPadding :: ImGuiWindowFlag
imGuiWindowFlagAlwaysUseWindowPadding = ImGuiWindowFlag $ 1 `shiftL` 16 --  Ensure child windows without border uses style.WindowPadding (ignored by default for non-bordered child windows, because more convenient)

imGuiWindowFlagNoNavInputs :: ImGuiWindowFlag
imGuiWindowFlagNoNavInputs            = ImGuiWindowFlag $ 1 `shiftL` 18 --  No gamepad/keyboard navigation within the window

imGuiWindowFlagNoNavFocus :: ImGuiWindowFlag
imGuiWindowFlagNoNavFocus             = ImGuiWindowFlag $ 1 `shiftL` 19 --  No focusing toward this window with gamepad/keyboard navigation (e.g. skipped by CTRL+TAB)

imGuiWindowFlagUnsavedDocument :: ImGuiWindowFlag
imGuiWindowFlagUnsavedDocument        = ImGuiWindowFlag $ 1 `shiftL` 20 --  Display a dot next to the title. When used in a tab/docking context, tab is selected when clicking the X + closure is not assumed (will wait for user to stop submitting the tab). Otherwise closure is assumed when pressing the X, so if you keep submitting the tab may reappear at end of tab bar.

imGuiWindowFlagNoDocking :: ImGuiWindowFlag
imGuiWindowFlagNoDocking              = ImGuiWindowFlag $ 1 `shiftL` 21 --  Disable docking of this window

imGuiWindowFlagNoNav :: ImGuiWindowFlag
imGuiWindowFlagNoNav                  = combineWindowFlags [imGuiWindowFlagNoNavInputs, imGuiWindowFlagNoNavFocus]

imGuiWindowFlagNoDecoration :: ImGuiWindowFlag
imGuiWindowFlagNoDecoration           = combineWindowFlags [imGuiWindowFlagNoTitleBar, imGuiWindowFlagNoResize, imGuiWindowFlagNoScrollbar, imGuiWindowFlagNoCollapse]

imGuiWindowFlagNoInputs :: ImGuiWindowFlag
imGuiWindowFlagNoInputs               = combineWindowFlags [imGuiWindowFlagNoMouseInputs, imGuiWindowFlagNoNavInputs, imGuiWindowFlagNoNavFocus]

-- Selectable flags
newtype ImGuiSelectableFlag = ImGuiSelectableFlag { selectableFlagValue :: CInt } deriving (Show, Eq)

combineSelectableFlags :: [ImGuiSelectableFlag] -> ImGuiSelectableFlag
combineSelectableFlags flags = ImGuiSelectableFlag $ foldr (\val acc -> acc .|. selectableFlagValue val) 0 flags

imGuiSelectableFlagNone :: ImGuiSelectableFlag
imGuiSelectableFlagNone               = ImGuiSelectableFlag 0

imGuiSelectableFlagDontClosePopups :: ImGuiSelectableFlag
imGuiSelectableFlagDontClosePopups    = ImGuiSelectableFlag $ 1 `shiftL` 0 -- Clicking this doesn't close parent popup window

imGuiSelectableFlagSpanAllColumns :: ImGuiSelectableFlag
imGuiSelectableFlagSpanAllColumns     = ImGuiSelectableFlag $ 1 `shiftL` 1 -- Selectable frame can span all columns (text will still fit in current column)

imGuiSelectableFlagAllowDoubleClick :: ImGuiSelectableFlag
imGuiSelectableFlagAllowDoubleClick   = ImGuiSelectableFlag $ 1 `shiftL` 2 -- Generate press events on double clicks too

imGuiSelectableFlagDisabled :: ImGuiSelectableFlag
imGuiSelectableFlagDisabled           = ImGuiSelectableFlag $ 1 `shiftL` 3 -- Cannot be selected, display grayed out text

