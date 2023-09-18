{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Enums
    ( WindowFlag(..)
    , SelectableFlag(..)
    , ItemFlag(..)
    , ItemStatusFlag(..)
    , fromFlag
    , combineFlags
    , hasFlag
    ) where
import           Data.Bits             (shiftL, (.&.), (.|.))
import           Foreign.C             (CInt)
import qualified Language.C.Inline     as C
import qualified Language.C.Inline.Cpp as Cpp

C.context Cpp.cppCtx
C.include "imgui.h"
Cpp.using "namespace ImGui";

class Enum a => ImguiFlag a where
    fromFlag :: a -> CInt
    fromFlag = (1 `shiftL`) . fromEnum
    combineFlags :: [a] -> CInt
    combineFlags = foldr (\ val acc -> acc .|. fromFlag val) 0
    hasFlag :: CInt -> a -> Bool
    hasFlag bitmask flag = (0 /=) $ bitmask .&. fromFlag flag

-- Window flags
data WindowFlag
    = WindowFlagNoTitleBar
    | WindowFlagNoResize
    | WindowFlagNoMove
    | WindowFlagNoScrollbar
    | WindowFlagNoScrollWithMouse
    | WindowFlagNoCollapse
    | WindowFlagAlwaysAutoResize
    | WindowFlagNoBackground
    | WindowFlagNoSavedSettings
    | WindowFlagNoMouseInputs
    | WindowFlagMenuBar
    | WindowFlagHorizontalScrollbar
    | WindowFlagNoFocusOnAppearing
    | WindowFlagNoBringToFrontOnFocus
    | WindowFlagAlwaysVerticalScrollbar
    | WindowFlagAlwaysHorizontalScrollbar
    | WindowFlagAlwaysUseWindowPadding
    | WindowFlagNoNavInputs
    | WindowFlagNoNavFocus
    | WindowFlagUnsavedDocument
    | WindowFlagNoDocking
    | WindowFlagNoNav
    | WindowFlagNoDecoration
    | WindowFlagNoInputs
    deriving (Show, Enum, Eq)

data SelectableFlag
    = SelectableFlagDontClosePopups
    | SelectableFlagSpanAllColumns
    | SelectableFlagAllowDoubleClick
    | SelectableFlagDisabled
    deriving (Eq, Enum)

data ItemFlag
  = ItemFlagNoTabStop
  | ItemFlagButtonRepeat
  | ItemFlagDisabled
  | ItemFlagNoNav
  | ItemFlagNoNavDefaultFocus
  | ItemFlagSelectableDontClosePopup
  | ItemFlagMixedValue
  | ItamFlagReadOnly
  | ItemFlagNoWindowHoverableCheck
  | ItemFlagAllowOverlap
  | ItamFlagInputable
  deriving (Eq, Enum)

data ItemStatusFlag
  = ItemStatusFlagHoveredRect
  | ItemStatusFlagHasDisplayRect
  | ItemStatusFlagEdited
  | ItemStatusFlagToggledSelection
  | ItemStatusFlagToggledOpen
  | ItemStatusFlagHasDeactivated
  | ItemStatusFlagDeactivated
  | ItemStatusFlagHoveredWindow
  | ItemStatusFlagFocusedByTabbing
  | ItemStatusFlagVisible
  | ItemStatusFlagOpenable
  | ItemStatusFlagOpened
  | ItemStatusFlagCheckable
  | ItemStatusFlagInputable
  deriving (Eq, Enum)

instance ImguiFlag WindowFlag
instance ImguiFlag SelectableFlag
instance ImguiFlag ItemFlag
instance ImguiFlag ItemStatusFlag
