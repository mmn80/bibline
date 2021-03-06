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
  , SortField(..)
  , SortOrder(..)
  , OutputFormat(..)
  , bibline
  , match
  ) where

import           Control.Monad       (unless)
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text, pack)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Pipes
import           Pipes.Parse         (runStateT)
import qualified Pipes.Prelude       as P
import qualified Pipes.Text          as PT
import qualified Pipes.Text.IO       as PT
import           System.IO           (hPrint, hPutStrLn, stderr)
import           System.Process      (spawnCommand)
import           Text.Bibline.Parser
import           Text.Read           (Lexeme (..), lexP, parens, readPrec)

data OutputFormat = Compact | BibTeX deriving (Eq)

data SortField = Unsorted | SortByTitle | SortByAuthor | SortByYear
  deriving (Eq)

instance Read SortField where
  readPrec =  parens $ do
    Ident s <- lexP
    return $ case s of
      "title"  -> SortByTitle
      "author" -> SortByAuthor
      "year"   -> SortByYear
      _        -> Unsorted

data SortOrder = SortAsc | SortDesc
  deriving (Eq)

instance Read SortOrder where
  readPrec =  parens $ do
    Ident s <- lexP
    return $ case s of
      "asc" -> SortAsc
      _     -> SortDesc

data Options = Options
  { optType      :: Maybe BibEntryType
  , optKey       :: String
  , optAuthor    :: String
  , optTitle     :: String
  , optYear      :: String
  , optTag       :: String
  , optSortBy    :: SortField
  , optSortOrder :: SortOrder
  , optFormat    :: OutputFormat
  , optOpen      :: Bool
  , optOpenCmd   :: String
  }

bibline :: Options -> IO ()
bibline opt@Options {..} = do
  let format = pack . if optFormat == Compact then showEntryCompact else show
  let noFilter = null optType && all null [optKey, optAuthor, optTitle, optYear, optTag]
  let fp = biblined PT.stdin
       >-> P.filter (if noFilter then const True else trickle opt)
  let runPipe p = runEffect $ for (p
              >-> (if optOpen then openFilePipe optOpenCmd else cat)
              >-> P.map format) $ liftIO . T.putStr
  (r, p) <- if optSortBy == Unsorted then runPipe fp else do
    (bs, r) <- P.toListM' fp
    let bs' = sortItems optSortBy optSortOrder bs
    runPipe $ each bs'
    return r
  unless (r == BibParseResultOk) $ do
    hPrint stderr r
    (used, p') <- runStateT PT.isEndOfChars p
    unless used $ do
      hPutStrLn stderr "Unused input:"
      runEffect $ for p' (liftIO . T.hPutStr stderr)

openFilePipe :: String -> Pipe BibItem BibItem IO r
openFilePipe cmd = do
  b <- await
  let f = stripParens $ bibFile b
  lift $ unless (T.null f) $ do
    _ <- spawnCommand $ cmd ++ " \"" ++ T.unpack f ++ "\""
    return ()
  yield b
  cat

sortItems :: SortField -> SortOrder -> [BibItem] -> [BibItem]
sortItems sf so = sortBy (cmp `on` fld)
  where cmp = if so == SortAsc then compare else flip compare
        fld = case sf of
                SortByAuthor -> T.intercalate (pack " and ")
                              . map (pack . show) . bibAuthor
                SortByTitle  -> bibTitle
                _            -> bibYear

trickle :: Options -> BibItem -> Bool
trickle Options {..} BibEntry {..} =
     (null optType   || fromJust optType == entryType)
  && (null optKey    || match (pack optKey) (stripParens entryKey))
  && (null optAuthor || any (match $ pack optAuthor) (pack . show <$> bibAuthor))
  && (null optTitle  || match (pack optTitle) (stripParens bibTitle))
  && (null optYear   || match (pack optYear) (stripParens bibYear))
  && (null optTag    || any (match $ pack optTag) bibKeywords)
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
