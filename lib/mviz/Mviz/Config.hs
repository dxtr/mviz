module Mviz.Config
  ( configDirectory
  , configFile
  , defaultConfig
  , ensureConfigDirectory
  , readConfig
  , parseConfig
  , configExists
  , fetchConfig
  , dumpConfig
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Mviz.Config.Types      (Config (..))
import           System.FilePath        (combine)
import           Toml                   (Result (Failure, Success), decode,
                                         encode)
import           UnliftIO.Directory     (XdgDirectory (XdgConfig),
                                         createDirectoryIfMissing,
                                         doesFileExist, getXdgDirectory)

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

writeConfig :: (MonadIO m) => FilePath -> Config -> m ()
writeConfig path = liftIO . writeFile path . show . encode

parseConfig :: String -> Either String Config
parseConfig content =
    case decode content of
        Failure []      -> Left "Unknown TOML error"
        Failure (e:_)   -> Left e
        Success (w:_) _ -> Left w
        Success [] c    -> Right c

configExists :: (MonadIO m) => FilePath -> m Bool
configExists = doesFileExist

fetchConfig :: (MonadIO m) => m (Either String Config)
fetchConfig = do
    cfgFile <- configFile "mviz.toml"
    cfgExists <- configExists cfgFile
    if cfgExists then
        readConfig cfgFile
    else do
        ensureConfigDirectory =<< configDirectory
        let cfg = defaultConfig
        writeConfig cfgFile cfg
        pure $ Right cfg

dumpConfig :: (MonadIO m) => Config -> m ()
dumpConfig config = do
    cfgFile <- configFile "mviz.toml"
    cfgExists <- configExists cfgFile
    if cfgExists then
        writeConfig cfgFile config
    else do
        ensureConfigDirectory =<< configDirectory
        let cfg = defaultConfig
        writeConfig cfgFile cfg
