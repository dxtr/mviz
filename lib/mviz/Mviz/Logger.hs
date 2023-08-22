{-# LANGUAGE OverloadedStrings #-}

module Mviz.Logger (runRingbufferLoggingT, LogMessage (..), logMessage) where

import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Logger     (Loc, LogLevel (..), LogSource,
                                           LogStr, LoggingT (runLoggingT),
                                           fromLogStr)
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Data.Time.Format
import           Data.Time.Format.ISO8601 (FormatExtension (ExtendedFormat),
                                           formatShow, hourMinuteFormat,
                                           iso8601Show, timeOfDayFormat)
import           Data.Time.LocalTime      (LocalTime (localTimeOfDay),
                                           ZonedTime (zonedTimeToLocalTime, zonedTimeZone),
                                           getZonedTime, utcToLocalTimeOfDay,
                                           utcToLocalZonedTime)
import           Data.Time.RFC3339
import qualified Mviz.Utils.Ringbuffer    as RB

data LogMessage = LogMessage ZonedTime Loc LogSource LogLevel LogStr
  deriving (Show)

logMessage :: LogMessage -> T.Text
logMessage (LogMessage zt loc logSource logLevel logStr) =
  "[" <> timestamp <> "][" <> levelStr <> "] " <> (T.decodeUtf8 $ fromLogStr logStr)
  where timestamp = formatTimeRFC3339 zt
        levelStr = case logLevel of
                     LevelDebug   -> "DEBUG"
                     LevelInfo    -> "INFO"
                     LevelWarn    -> "WARNING"
                     LevelError   -> "ERROR"
                     LevelOther l -> l

ringBufferOutput
  :: RB.Ringbuffer LogMessage -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
ringBufferOutput buffer loc src lvl str = do
  timestamp <- getZonedTime
  RB.put buffer $ LogMessage timestamp loc src lvl str

runRingbufferLoggingT
  :: (MonadIO m) => RB.Ringbuffer LogMessage -> LoggingT m a -> m a
runRingbufferLoggingT ringBuffer = (`runLoggingT` ringBufferOutput ringBuffer)
