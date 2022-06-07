{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Commonmark (commonmark)
import qualified Commonmark
import Commonmark.Pandoc (Cm (..))
import Control.Applicative (many, (<|>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer.Strict (runWriterT)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (for_, traverse_)
import qualified Data.List as List
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Pandoc.Builder (Block, Blocks, Inline (Link), Target)
import qualified Text.Pandoc.Walk as Pandoc

data Cli
  = Move FilePath FilePath
  | Check [FilePath]
  deriving (Eq, Show)

moveParser :: Options.Parser Cli
moveParser =
  Move
    <$> Options.strArgument (Options.metavar "SRC")
    <*> Options.strArgument (Options.metavar "DEST")

checkParser :: Options.Parser Cli
checkParser =
  Check
    <$> many (Options.strArgument $ Options.metavar "FILES")

cliParser :: Options.Parser Cli
cliParser =
  Options.subparser (Options.command "move" $ Options.info moveParser Options.fullDesc)
    <|> Options.subparser (Options.command "check" $ Options.info checkParser Options.fullDesc)

move :: FilePath -> FilePath -> IO ()
move from to = undefined

data CheckError
  = ParseError Commonmark.ParseError
  | BrokenLink FilePath Text
  deriving (Eq, Show)

data CheckInfo
  = CheckInfo (Sum Int) (DList CheckError)

instance Semigroup CheckInfo where
  CheckInfo a b <> CheckInfo a' b' = CheckInfo (a <> a') (b <> b')

instance Monoid CheckInfo where
  mempty = CheckInfo mempty mempty

checkBlocks :: (MonadWriter CheckInfo m, MonadIO m) => FilePath -> FilePath -> Blocks -> m ()
checkBlocks directory file blocks =
  for_ blocks $ \block ->
    for_ (Pandoc.query @Inline @Block asLink block) $ \(url, name) -> do
      when (isLocalFile url) $ do
        exists <- liftIO $ Directory.doesFileExist (directory </> Text.unpack url)
        unless exists . tell $ CheckInfo mempty (DList.singleton $ BrokenLink file url)
  where
    asLink :: Inline -> [Target]
    asLink (Link _ _ target) = [target]
    asLink _ = []

    isLocalFile :: Text -> Bool
    isLocalFile url =
      maybe True (const False) (Text.stripPrefix "http" url)
        && maybe True (const False) (Text.stripPrefix "mailto" url)

checkFile :: (MonadWriter CheckInfo m, MonadIO m) => FilePath -> m ()
checkFile file = do
  content <- liftIO $ Text.IO.readFile file
  case commonmark file content of
    Left err -> liftIO . putStrLn $ "parse error: " <> show err
    Right (blocks :: Cm () Blocks) -> do
      tell $ CheckInfo (Sum 1) mempty
      let directory = FilePath.takeDirectory file
      checkBlocks directory file $ unCm blocks

check :: [FilePath] -> IO ()
check files = do
  ((), CheckInfo (Sum filesChecked) errorsDList) <- runWriterT $ traverse_ checkFile files

  let errors = DList.toList errorsDList
  let errorCount = length errors

  for_ errors $ \err ->
    case err of
      ParseError err' -> print err'
      BrokenLink file url ->
        putStrLn $ file <> ": " <> "broken link: " <> Text.unpack url

  putStrLn $
    show filesChecked
      <> " "
      <> pluralise "file" filesChecked
      <> " checked, "
      <> show errorCount
      <> " "
      <> pluralise "error" errorCount
      <> " detected."

  if errorCount == 0
    then exitSuccess
    else exitFailure
  where
    pluralise word count = if count == 1 then word else word <> "s"

main :: IO ()
main = do
  cli <- Options.execParser (Options.info cliParser Options.fullDesc)
  case cli of
    Move from to -> move from to
    Check files -> check files