{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Bibline
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : MIT
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Backend for the bibline console utility.
-----------------------------------------------------------------------------

module Text.Bibline
  ( module Text.Bibline.Parser
  , Options(..)
  , SortOrder(..)
  , OutputFormat(..)
  , bibline
  ) where

import           Control.Monad       (unless)
import           Data.Text           (pack)
import qualified Data.Text.IO        as T
import           Pipes
import           Pipes.Parse         (runStateT)
import qualified Pipes.Prelude       as P
import qualified Pipes.Text          as PT
import qualified Pipes.Text.IO       as PT
import           System.IO           (hPrint, hPutStrLn, stderr)
import           Text.Bibline.Parser
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

bibline :: Options -> IO ()
bibline Options {..} = do
  let format = if optFormat == Compact then showEntryCompact else show
  (r, p) <- runEffect $
              for (biblined PT.stdin >-> P.map (pack . format)) $
                liftIO . T.putStr
  unless (r == BibParseResultOk) $ do
    hPrint stderr r
    (used, p') <- runStateT PT.isEndOfChars p
    unless used $ do
      hPutStrLn stderr "Unused input:"
      runEffect $ for p' (liftIO . T.hPutStr stderr)
