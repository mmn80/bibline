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
  ) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

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
  showsPrec _ (BibGenericEntry s) = showString $ '@':T.unpack s

data PersonName = PersonName { firstName :: Text
                             , lastName  :: Text
                             }

instance Show PersonName where
  showsPrec _ (PersonName fs lt) = showString (T.unpack lt)
    . showString ", " . showString (T.unpack fs)

data BibItem =
  BibEntry {
      entryKey :: Text
    , entryType :: BibEntryType
    -- | Publisher's address (usually just the city, but can be the full address for lesser-known publishers)
    , bibAddress :: Text
    -- | An annotation for annotated bibliography styles (not typical)
    , bibAnnote :: Text
    -- | The name(s) of the author(s) (in the case of more than one author, separated by and)
    , bibAuthor :: [PersonName]
    -- | The title of the book, if only part of it is being cited
    , bibBookTitle :: Text
    -- | The chapter number
    , bibChapter :: Text
    -- | The key of the cross-referenced entry
    , bibCrossRef :: Text
    -- | The edition of a book, long form (such as "First" or "Second")
    , bibEdition :: Text
    -- | The name(s) of the editor(s)
    , bibEditor :: [PersonName]
    -- | How it was published, if the publishing method is nonstandard
    , bibHowPublished :: Text
    -- | The institution that was involved in the publishing, but not necessarily the publisher
    , bibInstitution :: Text
    -- | The journal or magazine the work was published in
    , bibJournal :: Text
    -- | A hidden field used for specifying or overriding the alphabetical order of entries (when the "author" and "editor" fields are missing). Note that this is very different from the key (mentioned just after this list) that is used to cite or cross-reference the entry.
    , bibKey :: Text
    -- | The month of publication (or, if unpublished, the month of creation)
    , bibMonth :: Text
    -- | Miscellaneous extra information
    , bibNote :: Text
    -- | The "(issue) number" of a journal, magazine, or tech-report, if applicable. (Most publications have a "volume", but no "number" field.)
    , bibNumber :: Text
    -- | The conference sponsor
    , bibOrganization :: Text
    -- | Page numbers, separated either by commas or double-hyphens.
    , bibPages :: Text
    -- | The publisher's name
    , bibPublisher :: Text
    -- | The school where the thesis was written
    , bibSchool :: Text
    -- | The series of books the book was published in (e.g. "The Hardy Boys" or "Lecture Notes in Computer Science")
    , bibSeries :: Text
    -- | The title of the work
    , bibTitle :: Text
    -- | The field overriding the default type of publication (e.g. "Research Note" for techreport, "{PhD} dissertation" for phdthesis, "Section" for inbook/incollection)
    , bibType :: Text
    -- | The volume of a journal or multi-volume book
    , bibVolume :: Text
    -- | The year of publication (or, if unpublished, the year of creation)
    , bibYear :: Text
    -- | Non-standard tags
    , bibExtraTags :: [(Text, Text)]
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
  showsPrec d e@(BibEntry k ty _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
    showsPrec d ty . showString "{" . showText k . showString ",\n"
    . tag2Str "address"      (bibAddress e)
    . tag2Str "annote"       (bibAnnote e)
    . tag2Str "author"       (personNames2Str $ bibAuthor e)
    . tag2Str "booktitle"    (bibBookTitle e)
    . tag2Str "chapter"      (bibChapter e)
    . tag2Str "crossref"     (bibCrossRef e)
    . tag2Str "edition"      (bibEdition e)
    . tag2Str "editor"       (personNames2Str $ bibEditor e)
    . tag2Str "howpublished" (bibHowPublished e)
    . tag2Str "institution"  (bibInstitution e)
    . tag2Str "journal"      (bibJournal e)
    . tag2Str "key"          (bibKey e)
    . tag2Str "month"        (bibMonth e)
    . tag2Str "note"         (bibNote e)
    . tag2Str "number"       (bibNumber e)
    . tag2Str "organization" (bibOrganization e)
    . tag2Str "pages"        (bibPages e)
    . tag2Str "publisher"    (bibPublisher e)
    . tag2Str "school"       (bibSchool e)
    . tag2Str "series"       (bibSeries e)
    . tag2Str "title"        (bibTitle e)
    . tag2Str "type"         (bibType e)
    . tag2Str "volume"       (bibVolume e)
    . tag2Str "year"         (bibYear e)
    . showString (unlines (xtag2str <$> bibExtraTags e))
    . showString "}\n\n"
    where
      xtag2str (t, v) = T.unpack t ++ " = {" ++ T.unpack v ++ "}"
      tag2Str t v =
        if T.null v then id
        else showString t . showString " = {" . showText v . showString "}"
      personNames2Str = T.pack . intercalate " and " . map show
  showsPrec _ (BibComment c) =
    showString "@comment {\n" . showText c . showString "\n}\n"
  showsPrec _ (BibString sid sv) =
    showString "@string {\n" . showText sid . showString " = {"
    . showText sv . showString "}\n}\n"
  showsPrec _ (BibPreamble p) =
    showString "@preamble {\n" . showText p . showString "\n}\n"

showText :: Text -> ShowS
showText = showString . T.unpack
