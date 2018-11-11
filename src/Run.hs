{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO App ()
run = do
  env <- ask
  logDebug $ fromString . argHsfiles $ appOptions env
  logInfo "We're inside the application!"
