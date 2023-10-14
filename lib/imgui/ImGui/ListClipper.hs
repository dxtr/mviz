module ImGui.ListClipper
  ( Raw.new
  , Raw.delete
  , Raw.begin
  , Raw.displayStart
  , Raw.displayEnd
  , Raw.step
  , withClipper
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified ImGui.Raw.ListClipper   as Raw
import           UnliftIO.Exception      (bracket)

withClipper :: (MonadUnliftIO m) => Int -> Float -> (Raw.ListClipper -> m ()) -> m ()
withClipper itemsCount itemsHeight = bracket
                   (initFunc itemsCount itemsHeight)
                   Raw.delete
  where
    initFunc :: (MonadUnliftIO m1) => Int -> Float -> m1 Raw.ListClipper
    initFunc count height = do
          clipper <- Raw.new
          Raw.begin clipper count height
          pure clipper

