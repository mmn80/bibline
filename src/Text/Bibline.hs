{-# LANGUAGE MultiWayIf      #-}
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
  , match
  ) where

import           Control.Monad       (unless)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text, pack)
import qualified Data.Text.IO        as T
import qualified Data.Text        as T
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
bibline opt@Options {..} = do
  let format = if optFormat == Compact then showEntryCompact else show
  let noFilter = null optType && all null [optKey, optAuthor, optTitle, optYear, optTag]
  let pipeline = biblined PT.stdin
             >-> P.filter (if noFilter then const True else trickle opt)
             >-> P.map (pack . format)
  (r, p) <- runEffect $ for pipeline $ liftIO . T.putStr
  unless (r == BibParseResultOk) $ do
    hPrint stderr r
    (used, p') <- runStateT PT.isEndOfChars p
    unless used $ do
      hPutStrLn stderr "Unused input:"
      runEffect $ for p' (liftIO . T.hPutStr stderr)

trickle :: Options -> BibItem -> Bool
trickle Options {..} BibEntry {..} =
  if | not $ null optType   -> fromJust optType == entryType
     | not $ null optKey    -> match (pack optKey) $ stripParens entryKey
     | not $ null optAuthor -> any (match $ pack optAuthor) $ map (pack . show) bibAuthor
     | not $ null optTitle  -> match (pack optTitle) $ stripParens bibTitle
     | not $ null optYear   -> match (pack optYear) $ stripParens bibYear
     | not $ null optTag    -> any (match $ pack optTag) bibKeywords
     | otherwise -> True
trickle _ _ = True

match :: Text -> Text -> Bool
match pat txt = go pats (T.toLower txt)
  where
    wildchar c = c == '?' || c == '*'
    pats = T.groupBy (\c1 c2 -> not $ wildchar c1 || wildchar c2) (T.toLower pat)
    go ps str = case ps of
          []    -> True
          p:ps' ->
            if | p == T.singleton '*' ->
                 case ps' of
                   []      -> True
                   p':ps'' -> let (_, str2) = T.breakOn p' str in
                              not (T.null str2) &&
                              go ps'' (T.drop (T.length p') str2)
               | p == T.singleton '?' -> case T.uncons str of
                                           Nothing      -> False
                                           Just (_, ss) -> go ps' ss
               | otherwise -> case T.stripPrefix p str of
                                Nothing   -> False
                                Just str' -> go ps' str'
