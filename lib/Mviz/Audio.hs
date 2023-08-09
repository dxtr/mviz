module Mviz.Audio (
  AudioMessage (..),
  runAudioSystem,
  shutdown,
) where

import Control.Concurrent.STM (
  STM,
  TChan,
  atomically,
  writeTChan,
  tryReadTChan,
 )

data AudioMessage
  = Quit

readChannel :: TChan AudioMessage -> STM (Maybe AudioMessage)
-- readChannel channel False = Just $ readTChan channel
readChannel channel = tryReadTChan channel

runAudioSystem
  :: TChan AudioMessage -> TChan AudioMessage -> IO (Either String ())
runAudioSystem writeChan readChan = do
  putStrLn "Running audio system"
  msg <- atomically $ readChannel readChan
  case msg of
    Just _ -> do
      putStrLn "Quitting audio system"
      return $ Right ()
    Nothing -> runAudioSystem writeChan readChan

shutdown :: TChan AudioMessage -> IO ()
shutdown writeChan = atomically $ writeTChan writeChan msg
 where
  msg = Quit
