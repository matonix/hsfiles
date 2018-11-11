{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hsfiles 
  ( extract
  , create
  ) where

import Import
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.FilePath
import RIO.Directory
import Data.List.Extra
import Safe
import System.Directory.Tree

-- | Hsfiles Internal Representation
type Hsfiles = Map FilePath Text

-- * High level functions

extract :: FilePath -> FilePath -> RIO App ()
extract hsfilesPath dirsPath = do
  logDebug $ "source: " <> fromString hsfilesPath
  logDebug $ "dest: " <> fromString dirsPath
  hsfiles <- liftIO $ readHsfiles hsfilesPath
  forM_ (Map.keys hsfiles) $ \path -> 
    logDebug $ "extract: " <> fromString path
  liftIO $ writeDirs dirsPath hsfiles

create :: FilePath -> FilePath -> RIO App ()
create hsfilesPath dirsPath = do
  logDebug $ "source: " <> fromString hsfilesPath
  logDebug $ "dest: " <> fromString dirsPath
  hsfiles <- liftIO $ readDirs dirsPath
  forM_ (Map.keys hsfiles) $ \path -> 
    logDebug $ "create: " <> fromString path
  liftIO $ writeHsfiles hsfilesPath hsfiles

-- * Low level functions

readHsfiles :: FilePath -> IO Hsfiles
readHsfiles path = fromText <$> readFileUtf8 path

writeHsfiles :: FilePath -> Hsfiles -> IO ()
writeHsfiles path = writeFileUtf8 path . toText

readDirs :: FilePath -> IO Hsfiles
readDirs path = fromDirTree path . dirTree 
  <$> readDirectoryWith readFileUtf8 path

-- | TODO: use DirTree
writeDirs :: FilePath -> Hsfiles -> IO ()
writeDirs base hsfiles = makeFiles $ consRelativePath where
  consRelativePath = Map.mapKeys (base </>) hsfiles
  makeFiles = mapM_ (uncurry writeFileWithDir) . Map.toList where
    writeFileWithDir path text = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFileUtf8 path text

-- * Pure functions

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
toText = utf8BuilderToText . mconcat . map displayEntry . Map.toList where
  displayEntry (path, content) = 
    "{-# START_FILE " <> display (Text.pack path) <> " #-}\n"
    <> display content

fromDirTree :: FilePath -> DirTree Text -> Hsfiles
fromDirTree path = Map.mapKeys (makeRelative path) 
  . Map.fromList 
  . flatten ""

flatten :: FilePath -> DirTree Text -> [(FilePath, Text)]
flatten base Dir{..} = concatMap (flatten (base </> name)) contents
flatten base File{..} = [(base </> name, file)]
flatten _base Failed{..} = throwM err