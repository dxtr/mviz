{-# LANGUAGE OverloadedStrings #-}

module Mviz.Shader
  ( shaderDirectory
  , Mviz.Shader.listShaders
  , readShader
  ) where

import           Control.Monad             (filterM, unless)
import           Control.Monad.Error.Class (catchError)
import           Control.Monad.Except      (MonadError, throwError)
import qualified Data.ByteString           as BS
import           Data.Char                 (toLower)
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import           Mviz.Shader.Types
import           Mviz.Utils                ((<&&>))
import           Mviz.Utils.Filesystem
import           Prelude                   hiding (pred)
import           System.FilePath           (isExtensionOf, makeValid,
                                            takeBaseName, (<.>), (</>))
import           UnliftIO                  (MonadIO, liftIO)
import           UnliftIO.Directory        (XdgDirectory (XdgData),
                                            doesDirectoryExist, doesFileExist,
                                            findFilesWith, getXdgDirectory,
                                            listDirectory)

shaderDirectory :: (MonadIO m) => m FilePath
shaderDirectory = getXdgDirectory XdgData "mviz/shaders"

shaderFileExtension :: String
shaderFileExtension = "glsl"

shaderFilePredicate :: FilePath -> Bool
shaderFilePredicate = isExtensionOf shaderFileExtension

shaderPredicate :: MonadIO m => FilePath -> FilePath -> m Bool
shaderPredicate shaderDir p = do
  doesDirectoryExist (shaderDir </> p) <&&> pure (not (isDotFile p))

shaderNameToFilePath :: (MonadIO m) => T.Text -> m FilePath
shaderNameToFilePath name = construct <$> shaderDirectory
  where nameStr = T.unpack name
        construct path = makeValid $ (path </> nameStr)

validShaderFiles :: [String]
validShaderFiles = ["vertex", "fragment", "geometry"]

isShaderFile :: String -> Bool
isShaderFile p = let lowerP = map toLower (takeBaseName p) in
  lowerP `elem` validShaderFiles
--  where lowerP = map toLower (takeBaseName p)

shaderFileNameToShaderType :: FilePath -> Maybe ShaderType
shaderFileNameToShaderType "vertex"   = Just Vertex
shaderFileNameToShaderType "fragment" = Just Fragment
shaderFileNameToShaderType "geometry" = Just Geometry
shaderFileNameToShaderType s          = Nothing

shaderFiles :: (MonadIO m) => FilePath -> m [(Maybe ShaderType, FilePath)]
shaderFiles path =
  listDirectory path
    >>= pure . map dirFunc . filter isShaderFile
  where
    dirFunc :: FilePath -> (Maybe ShaderType, FilePath)
    dirFunc d = (shaderFileNameToShaderType (map toLower d), path </> d)

isDotFile :: String -> Bool
isDotFile = isPrefixOf "."

listShaders :: (MonadIO m) => m [T.Text]
listShaders = shaderDirectory >>= \shaderDir -> do
  let pred = shaderPredicate shaderDir
  ensureDirectory shaderDir
  map T.pack <$>
    (filterM pred =<< listDirectory shaderDir)

readShader :: (MonadIO m, MonadError ShaderError m)  => T.Text -> m Shader
readShader name = do
  shaderPath <- shaderNameToFilePath name
  directoryExists <- doesDirectoryExist shaderPath
  unless directoryExists $ throwError $ ShaderSourceDoesNotExist (name, shaderPath)
  sourceFiles <- shaderFiles shaderPath >>= pure . filter (\(t, _) -> isJust t)
  sources <- mapM ssFunc sourceFiles
  return $ Shader { shaderName = name, shaderSources = sources }
  where
    ssFunc (Just typ, path) = do
      content <- liftIO $ BS.readFile path
      pure $ ShaderSource { shaderSourceType = typ, shaderSourceData = content }
