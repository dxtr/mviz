module Mviz.Logger (runRingbufferLoggingT, LogMessage (..)) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (
  Loc,
  LogLevel,
  LogSource,
  LogStr,
  LoggingT (runLoggingT),
 )
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Mviz.Utils.Ringbuffer qualified as RB

data LogMessage = LogMessage ZonedTime Loc LogSource LogLevel LogStr

-- defaultOutput
--   :: Handle
--   -> Loc
--   -> LogSource
--   -> LogLevel
--   -> LogStr
--   -> IO ()
-- defaultOutput h loc src level msg =
--   S8.hPutStr h ls
--  where
--   ls = defaultLogStrBS loc src level msg

ringBufferOutput
  :: RB.Ringbuffer LogMessage -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
ringBufferOutput buffer loc src lvl str = do
  timestamp <- getZonedTime
  RB.put buffer $ LogMessage timestamp loc src lvl str

runRingbufferLoggingT
  :: (MonadIO m) => RB.Ringbuffer LogMessage -> LoggingT m a -> m a
runRingbufferLoggingT ringBuffer = (`runLoggingT` ringBufferOutput ringBuffer)
