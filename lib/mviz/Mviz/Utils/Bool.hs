{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Mviz.Utils.Bool where

newtype BoolT m = BoolT { runBoolT :: m (Bool) }
  deriving (Generic)

