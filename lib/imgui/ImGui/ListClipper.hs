module ImGui.ListClipper
  ( Raw.new
  , Raw.delete
  , Raw.begin
  , Raw.displayStart
  , Raw.displayEnd
  , Raw.step
  , withClipper
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified ImGui.Raw.ListClipper   as Raw
import           UnliftIO.Exception      (bracket, bracket_)

withClipper :: (MonadUnliftIO m) => Int -> Float -> (Raw.ListClipper -> m ()) -> m ()
withClipper itemsCount itemsHeight func = bracket
                   (initFunc itemsCount itemsHeight)
                   Raw.delete
                   func
  where
    initFunc :: (MonadUnliftIO m1) => Int -> Float -> m1 Raw.ListClipper
    initFunc count height = do
          clipper <- Raw.new
          Raw.begin clipper count height
          return clipper

