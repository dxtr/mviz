module Mviz.Shader.Types (Shader (..), ShaderError (..)) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

data ShaderError =
  ShaderSourceDoesNotExist (T.Text, FilePath)

data Shader = Shader { shaderName   :: T.Text
                     , shaderSource :: BS.ByteString
                     } deriving (Show)

