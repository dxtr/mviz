module Mviz.Config
  ( configDirectory
  , configFile
  , defaultConfig
  , ensureConfigDirectory
  , readConfig
  , parseConfig
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Mviz.Config.Types      (Config (..))
import           System.FilePath        (combine)
import           Toml                   (Result (Failure, Success), decode)
import           UnliftIO.Directory     (XdgDirectory (XdgConfig),
                                         createDirectoryIfMissing,
                                         getXdgDirectory)

configDirectory :: (MonadIO m) => m FilePath
configDirectory = getXdgDirectory XdgConfig "mviz"

configFile :: (MonadIO m) => FilePath -> m FilePath
configFile fileName = flip combine fileName <$> configDirectory

defaultConfig :: Config
defaultConfig = Config { configInputs = []
                       , configShowUI = False
                       }

ensureConfigDirectory :: (MonadIO m) => FilePath -> m ()
ensureConfigDirectory = createDirectoryIfMissing True

readConfig :: (MonadIO m) => FilePath -> m (Either String Config)
readConfig path = do
    content <- liftIO $ readFile path
    return $ parseConfig content

parseConfig :: String -> Either String Config
parseConfig content =
    case decode content of
        Failure []      -> Left "Unknown TOML error"
        Failure (e:_)   -> Left e
        Success (w:_) _ -> Left w
        Success [] c    -> Right c
