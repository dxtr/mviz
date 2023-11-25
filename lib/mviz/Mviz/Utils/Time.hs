module Mviz.Utils.Time (formatTimeRFC3339) where

import           Data.String         (fromString)
import qualified Data.Text           as T
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (ZonedTime (ZonedTime),
                                      timeZoneOffsetString, utc)

formatTimeRFC3339 :: ZonedTime -> T.Text
formatTimeRFC3339 zt@(ZonedTime _ z) = fromString (formatTime defaultTimeLocale "%FT%T" zt) <> fromString printZone
  where timeZoneStr = timeZoneOffsetString z
        printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr <> ":" <> drop 3 timeZoneStr
