module ImGui.Raw.Utils
  ( tWithCString
  , outmarshalBool
  ) where

import qualified Data.Text             as T
import           Data.Text.Foreign     (withCString)
import           Foreign.C.String      (CString)
import           Foreign.C.Types       (CUChar)
import           Foreign.Marshal.Utils (toBool)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)

tWithCString :: T.Text -> (CString -> IO a) -> IO a
tWithCString = withCString

outmarshalBool :: Ptr CUChar -> IO Bool
outmarshalBool ptr = toBool <$> peek ptr
