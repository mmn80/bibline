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

import Options.Applicative
import Text.Bibline

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
