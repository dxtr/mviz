{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui.Raw.ListClipper
  ( ImGuiListClipper
  , ListClipper
  , new
  , delete
  , begin
  , displayStart
  , displayEnd
  , step) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.C
import           ImGui.Raw.Context
import           ImGui.Raw.Types        (ImGuiListClipper, ListClipper)
import qualified Language.C.Inline      as C
import qualified Language.C.Inline.Cpp  as Cpp

C.context (Cpp.cppCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"

new :: (MonadIO m) => m ListClipper
new = liftIO [C.exp| ImGuiListClipper* { IM_NEW(ImGuiListClipper) } |]

delete :: (MonadIO m) => ListClipper -> m ()
delete clipper = liftIO [C.exp| void { IM_DELETE($(ImGuiListClipper* clipper)) } |]

begin :: (MonadIO m) => ListClipper -> Int -> Float -> m ()
begin clipper itemsCount itemsHeight =
  liftIO [C.block| void { $(ImGuiListClipper* clipper)->Begin($(int itemsCount'), $(float itemsHeight')); } |]
  where itemsCount' :: CInt = fromIntegral itemsCount
        itemsHeight' = CFloat itemsHeight

displayStart :: (MonadIO m) => ListClipper -> m CInt
displayStart clipper = liftIO [C.exp| int { $(ImGuiListClipper* clipper)->DisplayStart } |]

displayEnd :: (MonadIO m) => ListClipper -> m CInt
displayEnd clipper = liftIO [C.exp| int { $(ImGuiListClipper* clipper)->DisplayEnd } |]

step :: (MonadIO m) => ListClipper -> m Bool
step clipper = liftIO $ (0 /=) <$> [C.block| bool { return $(ImGuiListClipper* clipper)->Step(); } |]
