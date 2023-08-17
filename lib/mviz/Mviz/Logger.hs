module Mviz.Logger (runRingbufferLoggingT, LogMessage (..)) where

import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Logger     (Loc, LogLevel, LogSource, LogStr,
                                           LoggingT (runLoggingT))
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601 (FormatExtension (ExtendedFormat),
                                           formatShow, hourMinuteFormat)
import           Data.Time.LocalTime      (LocalTime (localTimeOfDay),
                                           ZonedTime (zonedTimeToLocalTime, zonedTimeZone),
                                           getZonedTime, utcToLocalTimeOfDay,
                                           utcToLocalZonedTime)
import qualified Mviz.Utils.Ringbuffer    as RB

data LogMessage = LogMessage ZonedTime Loc LogSource LogLevel LogStr

instance Show LogMessage where
  show (LogMessage zt loc logSource logLevel logStr) =
    "[" <> timestamp <> "][" <> show logLevel <> "]" <> show logStr
    where
      timezone = zonedTimeZone zt
      localTime = localTimeOfDay $ zonedTimeToLocalTime zt
      timestampFormat = hourMinuteFormat ExtendedFormat
      timestamp = formatShow timestampFormat localTime

ringBufferOutput
  :: RB.Ringbuffer LogMessage -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
ringBufferOutput buffer loc src lvl str = do
  timestamp <- getZonedTime
  RB.put buffer $ LogMessage timestamp loc src lvl str

runRingbufferLoggingT
  :: (MonadIO m) => RB.Ringbuffer LogMessage -> LoggingT m a -> m a
runRingbufferLoggingT ringBuffer = (`runLoggingT` ringBufferOutput ringBuffer)
