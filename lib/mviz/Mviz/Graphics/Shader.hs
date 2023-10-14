module Mviz.Graphics.Shader (
  ProgramObject (..),
  createProgram,
  activeUniforms,
) where

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.ByteString            as B
import           Graphics.Rendering.OpenGL  (($=!))
import qualified Graphics.Rendering.OpenGL  as GL

type Shader = GL.Shader

type ShaderType = GL.ShaderType

type Program = GL.Program

type UniformSpec = (GL.GLint, GL.VariableType, String)

data ShaderSource = ShaderSource
  { shaderType   :: ShaderType
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
  pure (if compiled then Right newShader else Left infoLog)

createShader' :: ShaderSource -> IO (Either String Shader)
createShader' ShaderSource{shaderType = sType, shaderSource = sSource} =
  createShader sType sSource

-- linkProgram :: GL.Program -> IO (Either String Program)
-- linkProgram program = do
--   return $ Left "Foo"

attachShaders :: Program -> [Shader] -> IO ()
attachShaders program = mapM_ (GL.attachShader program)

activeUniforms :: Program -> IO [UniformSpec]
activeUniforms = GL.get . GL.activeUniforms

createProgram :: [ShaderSource] -> IO (Either String Program)
createProgram shaderSources = do
  newProgram <- GL.createProgram
  shaderResult <- runExceptT $ mapM (ExceptT . createShader') shaderSources
  pure $ case attachShaders newProgram <$> shaderResult of
    Left msg -> Left msg
    Right _  -> Right newProgram

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
