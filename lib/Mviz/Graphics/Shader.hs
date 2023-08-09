module Mviz.Graphics.Shader (ProgramObject(..)) where

import Control.Monad.Except (ExceptT (..), MonadError (throwError), runExceptT)
import Data.ByteString qualified as B
import Graphics.Rendering.OpenGL (($=!))
import Graphics.Rendering.OpenGL qualified as GL

type Shader = GL.Shader

type ShaderType = GL.ShaderType

type Program = GL.Program
type UniformSpec = (GL.GLint, GL.VariableType, String)

data ShaderSource = ShaderSource
  { shaderType :: ShaderType
  , shaderSource :: B.ByteString
  }
  deriving (Show)

data ProgramObject = ProgramObject
  { programObjectId :: Int
  , programUniforms :: [UniformSpec]
  }
  deriving (Show)

compileStatus :: GL.Shader -> IO Bool
compileStatus = GL.get . GL.compileStatus

shaderInfoLog :: GL.Shader -> IO String
shaderInfoLog = GL.get . GL.shaderInfoLog

createShader :: GL.ShaderType -> B.ByteString -> IO (Either String Shader)
createShader sType sSource = do
  newShader <- GL.createShader sType
  GL.shaderSourceBS newShader $=! sSource
  GL.compileShader newShader
  compiled <- compileStatus newShader
  infoLog <- shaderInfoLog newShader
  return $ case compiled of
    False -> Left infoLog
    True -> Right newShader

createShader' :: ShaderSource -> IO (Either String Shader)
createShader' ShaderSource{shaderType = sType, shaderSource = sSource} =
  createShader sType sSource

linkProgram :: GL.Program -> IO (Either String Program)
linkProgram program = do
  return $ Left "Foo"

attachShaders :: Program -> [Shader] -> IO ()
attachShaders program = mapM_ (GL.attachShader program)

activeUniforms :: Program -> IO [UniformSpec]
activeUniforms = GL.get . GL.activeUniforms

-- (Int -> Bool) ->

createProgram :: [ShaderSource] -> IO (Either String Program)
createProgram shaderSources = do
  newProgram <- GL.createProgram
  shaderResult <- (runExceptT $ mapM (ExceptT . createShader') shaderSources)
  return $ case fmap (attachShaders newProgram) shaderResult of
    Left msg -> Left msg
    Right _ -> Right newProgram

--  return $ Right newProgram
-- \shaders_ -> fmap $ (do
--   shaders <- shaders_
--   attachShaders newProgram shaders
--   return $ newProgram)
-- return $ case shaders_ of
--   Right shaders -> do
--     --_ <- liftM $ attachShaders newProgram shaders
--     return newProgram
--   Left err -> throwError err
