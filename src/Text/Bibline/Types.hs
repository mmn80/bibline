{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Bibline.Types
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : MIT
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- BibTeX parse types.
-----------------------------------------------------------------------------

module Text.Bibline.Types
  (
    BibItem (..)
  , BibEntryType (..)
  , PersonName (..)
  , emptyEntry
  , showEntryCompact
  , stripParens
  ) where

import           Data.List   (intercalate)
import           Data.Monoid ((<>))
import           Data.Text   (Text, empty, pack, unpack)
import qualified Data.Text   as T
import           Text.Read   (Lexeme (..), lexP, parens, readPrec)

data BibEntryType
  -- | An article from a journal or magazine.
  -- Required fields: author, title, journal, year, volume
  -- Optional fields: number, pages, month, note, key
  = BibArticle
  -- | A book with an explicit publisher.
  -- Required fields: author/editor, title, publisher, year
  -- Optional fields: volume/number, series, address, edition, month, note, key
  | BibBook
  -- | A work that is printed and bound, but without a named publisher or sponsoring institution.
  -- Required fields: title
  -- Optional fields: author, howpublished, address, month, year, note, key
  | BibBooklet
  -- | The same as inproceedings, included for Scribe compatibility.
  | BibConference
  -- | A part of a book, usually untitled. May be a chapter (or section, etc.) and/or a range of pages.
  -- Required fields: author/editor, title, chapter/pages, publisher, year
  -- Optional fields: volume/number, series, type, address, edition, month, note, key
  | BibInBook
  -- | A part of a book having its own title.
  -- Required fields: author, title, booktitle, publisher, year
  -- Optional fields: editor, volume/number, series, type, chapter, pages, address, edition, month, note, key
  | BibInCollection
  -- | An article in a conference proceedings.
  -- Required fields: author, title, booktitle, year
  -- Optional fields: editor, volume/number, series, pages, address, month, organization, publisher, note, key
  | BibInProceedings
  -- | Technical documentation.
  -- Required fields: title
  -- Optional fields: author, organization, address, edition, month, year, note, key
  | BibManual
  -- | A Master's thesis.
  -- Required fields: author, title, school, year
  -- Optional fields: type, address, month, note, key
  | BibMastersThesis
  -- | For use when nothing else fits.
  -- Required fields: none
  -- Optional fields: author, title, howpublished, month, year, note, key
  | BibMisc
  -- | A Ph.D. thesis.
  -- Required fields: author, title, school, year
  -- Optional fields: type, address, month, note, key
  | BibPhdThesis
  -- | The proceedings of a conference.
  -- Required fields: title, year
  -- Optional fields: editor, volume/number, series, address, month, publisher, organization, note, key
  | BibProceedings
  -- | A report published by a school or other institution, usually numbered within a series.
  -- Required fields: author, title, institution, year
  -- Optional fields: type, number, address, month, note, key
  | BibTechReport
  -- | A document having an author and title, but not formally published.
  -- Required fields: author, title, note
  -- Optional fields: month, year, key
  | BibUnpublished
  -- | Non-standard entry type
  | BibGenericEntry Text

instance Show BibEntryType where
  showsPrec _ BibArticle          = showString "@article"
  showsPrec _ BibBook             = showString "@book"
  showsPrec _ BibBooklet          = showString "@booklet"
  showsPrec _ BibConference       = showString "@conference"
  showsPrec _ BibInBook           = showString "@inbook"
  showsPrec _ BibInCollection     = showString "@incollection"
  showsPrec _ BibInProceedings    = showString "@inproceedings"
  showsPrec _ BibManual           = showString "@manual"
  showsPrec _ BibMastersThesis    = showString "@mastersthesis"
  showsPrec _ BibMisc             = showString "@misc"
  showsPrec _ BibPhdThesis        = showString "@phdthesis"
  showsPrec _ BibProceedings      = showString "@proceedings"
  showsPrec _ BibTechReport       = showString "@techreport"
  showsPrec _ BibUnpublished      = showString "@unpublished"
  showsPrec _ (BibGenericEntry s) = showString $ '@':unpack s

instance Read BibEntryType where
  readPrec =  parens $ do
    Ident s <- lexP
    return $ case s of
      "article"       -> BibArticle
      "book"          -> BibBook
      "booklet"       -> BibBooklet
      "conference"    -> BibConference
      "inbook"        -> BibInBook
      "incollection"  -> BibInCollection
      "inproceedings" -> BibInProceedings
      "manual"        -> BibManual
      "mastersthesis" -> BibMastersThesis
      "misc"          -> BibMisc
      "phdthesis"     -> BibPhdThesis
      "proceedings"   -> BibProceedings
      "techreport"    -> BibTechReport
      "unpublished"   -> BibUnpublished
      ty              -> BibGenericEntry $ pack ty

data PersonName = PersonName { firstName  :: Text
                             , middleName :: Text
                             , lastName   :: Text
                             , nameSuffix :: Text
                             }

instance Show PersonName where
  showsPrec _ (PersonName fn mn ln sf) =
      showString (unpack ln) . showString ", "
    . (if T.null sf then id else showString (unpack sf) . showString ", ")
    . showString (unpack fn)
    . (if T.null mn then id else showString " " . showString (unpack mn))

data BibItem =
  BibEntry {
      entryKey        :: Text
    , entryType       :: BibEntryType
    -- | Publisher's address (usually just the city, but can be the full address for lesser-known publishers)
    , bibAddress      :: Text
    -- | An annotation for annotated bibliography styles (not typical)
    , bibAnnote       :: Text
    -- | The name(s) of the author(s) (in the case of more than one author, separated by and)
    , bibAuthor       :: [PersonName]
    -- | The title of the book, if only part of it is being cited
    , bibBookTitle    :: Text
    -- | The chapter number
    , bibChapter      :: Text
    -- | The key of the cross-referenced entry
    , bibCrossRef     :: Text
    -- | The edition of a book, long form (such as "First" or "Second")
    , bibEdition      :: Text
    -- | The name(s) of the editor(s)
    , bibEditor       :: [PersonName]
    -- | How it was published, if the publishing method is nonstandard
    , bibHowPublished :: Text
    -- | The institution that was involved in the publishing, but not necessarily the publisher
    , bibInstitution  :: Text
    -- | The journal or magazine the work was published in
    , bibJournal      :: Text
    -- | A hidden field used for specifying or overriding the alphabetical order of entries (when the "author" and "editor" fields are missing). Note that this is very different from the key (mentioned just after this list) that is used to cite or cross-reference the entry.
    , bibKey          :: Text
    -- | The month of publication (or, if unpublished, the month of creation)
    , bibMonth        :: Text
    -- | Miscellaneous extra information
    , bibNote         :: Text
    -- | The "(issue) number" of a journal, magazine, or tech-report, if applicable. (Most publications have a "volume", but no "number" field.)
    , bibNumber       :: Text
    -- | The conference sponsor
    , bibOrganization :: Text
    -- | Page numbers, separated either by commas or double-hyphens.
    , bibPages        :: Text
    -- | The publisher's name
    , bibPublisher    :: Text
    -- | The school where the thesis was written
    , bibSchool       :: Text
    -- | The series of books the book was published in (e.g. "The Hardy Boys" or "Lecture Notes in Computer Science")
    , bibSeries       :: Text
    -- | The title of the work
    , bibTitle        :: Text
    -- | The field overriding the default type of publication (e.g. "Research Note" for techreport, "{PhD} dissertation" for phdthesis, "Section" for inbook/incollection)
    , bibType         :: Text
    -- | The volume of a journal or multi-volume book
    , bibVolume       :: Text
    -- | The year of publication (or, if unpublished, the year of creation)
    , bibYear         :: Text
    -- | Non-standard tags
    , bibExtraTags    :: [(Text, Text)]
    }
  -- | @COMMENT for comments not taken in regard by BibTeX.
  | BibComment Text
  -- | `@STRING` defines abbreviations in the form of
  -- `@string { foo = "Mrs. Foo" }`
  -- which can later be used in a tag like this
  -- `author = foo # " and Mr. Bar"`
  | BibString { stringId    :: Text
              , stringValue :: Text
              }
  -- | @PREAMBLE defines how special text should be formatted.
  | BibPreamble Text

instance Show BibItem where
  showsPrec d BibEntry {..} =
    showsPrec d entryType . showString "{" . showText entryKey
    . tag2Str "address"      bibAddress
    . tag2Str "annote"       bibAnnote
    . tag2Str "author"       (personNames2Str bibAuthor)
    . tag2Str "booktitle"    bibBookTitle
    . tag2Str "chapter"      bibChapter
    . tag2Str "crossref"     bibCrossRef
    . tag2Str "edition"      bibEdition
    . tag2Str "editor"       (personNames2Str bibEditor)
    . tag2Str "howpublished" bibHowPublished
    . tag2Str "institution"  bibInstitution
    . tag2Str "journal"      bibJournal
    . tag2Str "key"          bibKey
    . tag2Str "month"        bibMonth
    . tag2Str "note"         bibNote
    . tag2Str "number"       bibNumber
    . tag2Str "organization" bibOrganization
    . tag2Str "pages"        bibPages
    . tag2Str "publisher"    bibPublisher
    . tag2Str "school"       bibSchool
    . tag2Str "series"       bibSeries
    . tag2Str "title"        bibTitle
    . tag2Str "type"         bibType
    . tag2Str "volume"       bibVolume
    . tag2Str "year"         bibYear
    . showString (concat (xtag2str <$> bibExtraTags))
    . showString "\n}\n\n"
    where
      xtag2str (t, v) = ",\n" ++ unpack t ++ " = " ++ unpack v
      tag2Str t v =
        if T.null v then id
        else showString ",\n" . showString t . showString " = " . showText v
      personNames2Str ps =
        if null ps then empty
        else pack . (++"}") . ('{':) . intercalate " and " $ map show ps
  showsPrec _ (BibComment c) =
    showString "@comment{\n" . showText c . showString "\n}\n\n"
  showsPrec _ (BibString sid sv) =
    showString "@string{\n" . showText sid . showString " = "
    . showText sv . showString "\n}\n\n"
  showsPrec _ (BibPreamble p) =
    showString "@preamble{\n" . showText p . showString "\n}\n\n"

showText :: Text -> ShowS
showText = showString . unpack

format :: Int -> Text -> String
format l txt = if | len < l -> str ++ replicate (l - len) ' '
                  | l > 4 && len > l - 2 -> take (l - 2) str ++ ".."
                  | len > l -> take l str
                  | otherwise -> str
  where str  = unpack txt'
        txt' = stripParens txt
        len  = T.length txt'

persons2Str :: [PersonName] -> Text
persons2Str = T.intercalate (pack ", ") . map lastName

showEntryCompact :: BibItem -> String
showEntryCompact BibEntry {..} = format 4 bibYear ++ " "
  ++ format 16 (persons2Str bibAuthor) ++ " " ++ format 58 bibTitle
showEntryCompact (BibComment c) = "comment: " ++ unpack c
showEntryCompact (BibString sid sv) = format 80 $ pack "string: " <> sid
                                               <> pack " = " <> sv
showEntryCompact (BibPreamble p) = format 80 $ pack "preamble" <> p

stripParens :: Text -> Text
stripParens txt = if T.compareLength txt 2 == GT
                  then let h = T.head txt
                           l = T.last txt in
                       if (h == '{' && l == '}') || (h == '"' && l == '"')
                       then T.dropEnd 1 $ T.drop 1 txt
                       else txt
                  else txt

emptyEntry :: BibItem
emptyEntry = BibEntry empty BibMisc empty empty [] empty empty empty empty []
  empty empty empty empty empty empty empty empty empty empty empty empty empty
  empty empty empty []
