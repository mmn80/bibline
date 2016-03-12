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
  , parsePersonNames
  ) where

import Pipes
import Pipes.Parse
import Data.Text (Text)
import Text.Bibline.Types

data BibParseResult = BibParseResultOk
                    | BibUnexpectedEndOfInput String
                    | BibSyntaxError String

biblined :: Monad m => Producer Text m r ->
                       Producer BibItem m (BibParseResult, Producer Text m r)
biblined = parsed itemParser

itemParser :: Monad m => Parser Text m (Either BibParseResult BibItem)
itemParser = do
  --s <- draw
  return $ Left BibParseResultOk

parsePersonNames :: String -> [PersonName]
parsePersonNames _ = []
