{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Run (run) where

import Import
import Hsfiles

run :: RIO App ()
run = do
  Options {..} <- appOptions <$> ask
  case (optionsCreate, optionsExtract) of
    (True, True) -> logError "invalid option"
    (True, False) -> do
      logDebug "create mode"
      create argHsfiles argDirs
    (False, True) -> do
      logDebug "extract mode"
      extract argHsfiles argDirs
    (False, False) -> logError "invalid option"
