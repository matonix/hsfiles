{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hsfiles 
  ( extract
  , create
  ) where

import Import
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.Directory
import RIO.FilePath
import Data.List.Extra
import Safe

-- | Hsfiles Internal Representation
type Hsfiles = Map FilePath Text

-- * High level functions

extract :: FilePath -> FilePath -> RIO App ()
extract hsfilesPath dirsPath = do
  current <- getCurrentDirectory
  logDebug $ "source: " <> fromString (current </> hsfilesPath)
  logDebug $ "dest: " <> fromString (current </> dirsPath)
  hsfiles <- liftIO $ readHsfiles (current </> hsfilesPath)
  liftIO $ writeDirs (current </> dirsPath) hsfiles

create :: FilePath -> FilePath -> RIO App ()
create hsfilesPath dirsPath = do
  current <- getCurrentDirectory
  logDebug $ "source: " <> fromString (current </> dirsPath)
  logDebug $ "dest: " <> fromString (current </> hsfilesPath)
  hsfiles <- liftIO $ readDirs (current </> dirsPath)
  liftIO $ writeHsfiles (current </> hsfilesPath) hsfiles

-- * Low level functions

readHsfiles :: FilePath -> IO Hsfiles
readHsfiles path = fromText <$> readFileUtf8 path

writeHsfiles :: FilePath -> Hsfiles -> IO ()
writeHsfiles path = writeFileUtf8 path . toText

readDirs :: FilePath -> IO Hsfiles
readDirs = undefined

writeDirs :: FilePath -> Hsfiles -> IO ()
writeDirs base hsfiles = makeFiles $ consRelativePath where
  consRelativePath = Map.mapKeys (base </>) hsfiles
  makeFiles = mapM_ (uncurry writeFileWithDir) . Map.toList where
    writeFileWithDir path text = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFileUtf8 path text

fromText :: Text -> Hsfiles
fromText = Map.fromList . uncurry zip . (paths &&& contents) . Text.lines where
  paths = map snapStartFile . filter isStartFile
  contents = tailSafe . map Text.unlines . split isStartFile
  isStartFile = Text.isPrefixOf "{-# START_FILE "
  snapStartFile = Text.unpack 
    . Text.strip
    . Text.dropPrefix "{-# START_FILE " 
    . Text.dropSuffix "#-}"

toText :: Hsfiles -> Text
toText = undefined


