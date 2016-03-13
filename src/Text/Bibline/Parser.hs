{-# LANGUAGE LambdaCase, MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Bibline.Parser
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : MIT
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Streaming BibTeX files parser.
-----------------------------------------------------------------------------

module Text.Bibline.Parser
  ( module Text.Bibline.Types
  , itemParser
  , biblined
  , BibParseResult (..)
  ) where

import Data.Char (isAlphaNum, isSeparator)
import Data.Text (Text, singleton, unpack)
import qualified Data.Text as T
import Control.Monad.Except
import Pipes
import Pipes.Parse
import Pipes.Text (isEndOfChars)
import Text.Bibline.Types

data BibParseResult = BibParseResultOk
                    | BibSyntaxError String

instance Show BibParseResult where
  show BibParseResultOk = "Parsing finished with success."
  show (BibSyntaxError msg) = "Parse error: " ++ msg

biblined :: Monad m => Producer Text m r ->
                       Producer BibItem m (BibParseResult, Producer Text m r)
biblined = parsed itemParser

type BibParser m r = ExceptT BibParseResult (StateT (Producer Text m r) m)

drawWord :: Monad m => BibParser m r Text
drawWord = go T.empty
  where go acc = lift draw >>= \case
          Nothing  -> return acc
          Just txt -> let (t1, t2) = T.span isAlphaNum txt in
            if | T.null t2 -> go $ acc `T.append` t1
               | T.null t1 && T.null acc -> let Just (c, t2') = T.uncons t2 in
                 putBack t2' >> return (singleton c)
               | otherwise -> putBack t2 >> return (acc `T.append` t1)
        putBack = lift . unDraw . T.dropWhile isSeparator

expect :: Monad m => Text -> BibParser m r ()
expect txt = do
  w <- drawWord
  unless (w == txt) $
    throwError $ BibSyntaxError $ "Expecting \"" ++ unpack txt
      ++ "\" but got " ++ if T.null w then "[END OF INPUT]"
                          else '"' : unpack w ++ "\""

expectChar :: Monad m => Char -> BibParser m r ()
expectChar = expect . singleton

drawBlock :: Monad m => BibParser m r Text
drawBlock = do
  expectChar '{'
  let go acc = do
        w <- drawWord
        if w == singleton '}' then return acc
        else go $ acc `T.append` (' ' `T.cons` w)
  go T.empty

drawTag :: Monad m => BibParser m r (Text, Text)
drawTag = (,) <$> drawWord <*> (expectChar '=' >> drawBlock)

itemParser :: Monad m => Parser Text m (Either BibParseResult BibItem)
itemParser = runExceptT $ do
  eof <- lift isEndOfChars
  when eof $ throwError BibParseResultOk
  expectChar '@'
  ty <- unpack . T.toLower <$> drawWord
  if | ty == "preamble" -> BibPreamble <$> drawBlock
     | ty == "comment"  -> BibComment <$> drawBlock
     | ty == "string"   -> do
       (t, v) <- drawTag
       return $ BibString t v
     | otherwise -> BibComment <$> drawBlock
