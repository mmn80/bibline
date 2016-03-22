{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : MIT
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Bibtex console utility.
-----------------------------------------------------------------------------

module Main (main) where

import           Control.Monad       (unless)
import           Data.Text           (pack)
import qualified Data.Text.IO        as T
import           Options.Applicative
import           Pipes
import           Pipes.Parse         (runStateT)
import qualified Pipes.Prelude       as P
import qualified Pipes.Text          as PT
import qualified Pipes.Text.IO       as PT
import           System.IO           (hPrint, hPutStrLn, stderr)
import           Text.Bibline
import           Text.Read           (Lexeme (..), lexP, parens, readPrec)

data OutputFormat = Compact | BibTeX deriving (Eq)

data SortOrder = Unsorted | SortByTitle | SortByAuthor | SortByYear

instance Read SortOrder where
  readPrec =  parens $ do
    Ident s <- lexP
    return $ case s of
      "title"  -> SortByTitle
      "author" -> SortByAuthor
      "year"   -> SortByYear
      _        -> Unsorted

data Options = Options
  { optType   :: Maybe BibEntryType
  , optKey    :: String
  , optAuthor :: String
  , optTitle  :: String
  , optYear   :: String
  , optTag    :: String
  , optSortBy :: SortOrder
  , optFormat :: OutputFormat
  }

maybeReader :: Read a => ReadM (Maybe a)
maybeReader = eitherReader $ \arg ->
  if arg == "" then return Nothing
  else case reads arg of
    [(r, "")] -> return (Just r)
    _         -> Left $ "cannot parse value '" ++ arg ++ "'"

optsParser :: Parser Options
optsParser = Options
     <$> option maybeReader
         ( long "type"
        <> value Nothing
        <> metavar "TYPE"
        <> help "Filter by entry type" )
     <*> strOption
         ( long "key"
        <> value ""
        <> metavar "KEY"
        <> help "Filter by citation key" )
     <*> strOption
         ( long "author"
        <> value ""
        <> metavar "AUTHOR"
        <> help "Filter by author name" )
     <*> strOption
         ( long "title"
        <> value ""
        <> metavar "TITLE"
        <> help "Filter by title" )
     <*> strOption
         ( long "year"
        <> value ""
        <> metavar "YEAR"
        <> help "Filter by publication year" )
     <*> strOption
         ( long "tag"
        <> value ""
        <> metavar "TAG"
        <> help "Filter by keyword" )
     <*> option auto
         ( long "sort"
        <> short 's'
        <> value Unsorted
        <> metavar "SORTBY"
        <> help "Sort results by title | author | year" )
     <*> flag Compact BibTeX
         ( long "bibtex"
        <> short 'b'
        <> help "Enable BibTeX output format" )

main :: IO ()
main = execParser opts >>= bibline
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
     <> progDesc "Reads BibTeX on stdin and outputs a possibly filtered and \
                   \sorted list of entries using either BibTeX or a \
                   \compact human friendly format"
     <> header "bibline - utility for processing BibTeX files" )

bibline :: Options -> IO ()
bibline Options {..} = do
  let format = if optFormat == Compact then showEntryCompact else show
  (r, p) <- runEffect $
              for (biblined PT.stdin >-> P.map (pack . format)) $
                liftIO . T.putStr
  unless (r == BibParseResultOk) $ hPrint stderr r
  (used, p') <- runStateT PT.isEndOfChars p
  unless used $ do
    hPutStrLn stderr "Unused input:"
    runEffect $ for p' (liftIO . T.hPutStr stderr)
