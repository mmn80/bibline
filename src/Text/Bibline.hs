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
  , bibItem2Text
  ) where

import Data.Text (Text, pack)
import Text.Bibline.Parser

tag2Str :: String -> String -> String
tag2Str t v = if null v then ""
              else t ++ " = {" ++ v ++ "}"

bibItem2Text :: BibItem -> Text
bibItem2Text e@(BibEntry k ty _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
  =  pack $ bibEntryType2Str ty ++ "{" ++ k ++ ",\n"
  ++ tag2Str "address" (bibAddress e)
  ++ tag2Str "annote" (bibAnnote e)
  ++ tag2Str "author" (personNames2Str $ bibAuthor e)
  ++ tag2Str "booktitle" (bibBookTitle e)
  ++ tag2Str "chapter" (bibChapter e)
  ++ tag2Str "crossref" (bibCrossRef e)
  ++ tag2Str "edition" (bibEdition e)
  ++ tag2Str "editor" (personNames2Str $ bibEditor e)
  ++ tag2Str "howpublished" (bibHowPublished e)
  ++ tag2Str "institution" (bibInstitution e)
  ++ tag2Str "journal" (bibJournal e)
  ++ tag2Str "key" (bibKey e)
  ++ tag2Str "month" (bibMonth e)
  ++ tag2Str "note" (bibNote e)
  ++ tag2Str "number" (bibNumber e)
  ++ tag2Str "organization" (bibOrganization e)
  ++ tag2Str "pages" (bibPages e)
  ++ tag2Str "publisher" (bibPublisher e)
  ++ tag2Str "school" (bibSchool e)
  ++ tag2Str "series" (bibSeries e)
  ++ tag2Str "title" (bibTitle e)
  ++ tag2Str "type" (bibType e)
  ++ tag2Str "volume" (bibVolume e)
  ++ tag2Str "year" (bibYear e)
  ++ unlines ((\(t, v) -> t ++ " = {" ++ v ++ "}") <$> bibExtraTags e)
  ++ "}\n\n"
bibItem2Text (BibComment c)
  = pack $ "@comment {\n" ++ c ++ "\n}\n"
bibItem2Text (BibString sid sv)
  = pack $ "@string {\n" ++ sid ++ " = {" ++ sv ++ "}\n}\n"
bibItem2Text (BibPreamble p)
  = pack $ "@preamble {\n" ++ p ++ "\n}\n"
