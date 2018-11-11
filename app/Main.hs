{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_hsfiles

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_hsfiles.version)
    "hsfiles: Convert *.hsfiles from/to actual directory structure using tar-like commands"
    empty
    (Options
      <$> switch 
        ( long "verbose"
        <> short 'v'
        <> help "Verbose output"
        )
      <*> switch 
        ( long "extract"
        <> short 'x'
        <> help "Extract hsfiles into directory structure"
        )
      <*> switch 
        ( long "create"
        <> short 'c'
        <> help "Create hsfiles from directory structure"
        )
      <*> strArgument 
        ( metavar "YOUR_TEMPLATE.hsfiles"
        <> help "File path of your template"
        )
      <*> strArgument 
        ( metavar "YOUR_TEMPLATE_DIR"
        <> help "Directory path of your template"
        )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
