module Mviz.Shader.Types
  ( Shader (..)
  , ShaderError (..)
  , ShaderType (..)
  , ShaderSource (..)
  , MonadShader (..)
  , HasShaders (..)
  ) where

import qualified Data.ByteString as BS
import           Data.Char       (toLower)
import qualified Data.Text       as T

data ShaderError = ShaderSourceDoesNotExist (T.Text, FilePath)
                 | InvalidShaderSource FilePath

data ShaderType = Vertex | Fragment | Geometry
  deriving (Show, Eq)

instance Read ShaderType where
  readsPrec _ "" = []
  readsPrec _ t =
    let tCase = map toLower t in
      case tCase of
        "fragment" -> [(Fragment, "")]
        "geometry" -> [(Geometry, "")]
        "vertex"   -> [(Vertex, "")]
        _          -> []

data ShaderSource = ShaderSource { shaderSourceType :: ShaderType
                                 , shaderSourceData :: BS.ByteString
                                 } deriving (Show)

data Shader = Shader { shaderName    :: T.Text
                     , shaderSources :: [ShaderSource]
                     } deriving (Show)

class HasShaders m where
  getShaders :: m -> IO [(Bool, T.Text)]

class Monad m => MonadShader m where
  listShaders :: m [T.Text]
  loadShader :: T.Text -> m Shader
