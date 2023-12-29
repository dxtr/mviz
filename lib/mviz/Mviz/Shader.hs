{-# LANGUAGE OverloadedStrings #-}

module Mviz.Shader
  ( shaderDirectory
  , listShaders
  , readShader
  ) where

import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError, throwError)
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import           Mviz.Shader.Types
import           Mviz.Utils.Filesystem
import           System.FilePath       (isExtensionOf, makeValid, takeBaseName,
                                        (<.>), (</>))
import           UnliftIO              (MonadIO, liftIO)
import           UnliftIO.Directory    (XdgDirectory (XdgData), doesFileExist,
                                        findFilesWith, getXdgDirectory,
                                        listDirectory)

shaderDirectory :: (MonadIO m) => m FilePath
shaderDirectory = getXdgDirectory XdgData "mviz/shaders"

shaderFileExtension :: String
shaderFileExtension = "glsl"

shaderFilePredicate :: FilePath -> Bool
shaderFilePredicate = isExtensionOf shaderFileExtension

shaderNameToFilePath :: (MonadIO m) => T.Text -> m FilePath
shaderNameToFilePath name = construct <$> shaderDirectory
  where nameStr = T.unpack name
        construct path = makeValid $ (path </> nameStr) <.> shaderFileExtension

listShaders :: (MonadIO m) => m [T.Text]
listShaders = shaderDirectory >>= \shaderDir -> do
  ensureDirectory shaderDir
  map (T.pack . takeBaseName) . filter shaderFilePredicate <$> listDirectory shaderDir

readShader :: (MonadIO m, MonadError ShaderError m)  => T.Text -> m Shader
readShader name = do
  shaderPath <- shaderNameToFilePath name
  fileExists <- doesFileExist shaderPath
  unless fileExists $ throwError $ ShaderSourceDoesNotExist (name, shaderPath)
  content <- liftIO $ BS.readFile shaderPath
  return $ Shader { shaderName = name, shaderSource = content }
