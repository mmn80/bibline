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
  , bibEntryType2Str
  , PersonName (..)
  , personNames2Str
  ) where

import Data.List (intercalate)

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
  | BibGenericEntry String

bibEntryType2Str :: BibEntryType -> String
bibEntryType2Str BibArticle          = "@article"
bibEntryType2Str BibBook             = "@book"
bibEntryType2Str BibBooklet          = "@booklet"
bibEntryType2Str BibConference       = "@conference"
bibEntryType2Str BibInBook           = "@inbook"
bibEntryType2Str BibInCollection     = "@incollection"
bibEntryType2Str BibInProceedings    = "@inproceedings"
bibEntryType2Str BibManual           = "@manual"
bibEntryType2Str BibMastersThesis    = "@mastersthesis"
bibEntryType2Str BibMisc             = "@misc"
bibEntryType2Str BibPhdThesis        = "@phdthesis"
bibEntryType2Str BibProceedings      = "@proceedings"
bibEntryType2Str BibTechReport       = "@techreport"
bibEntryType2Str BibUnpublished      = "@unpublished"
bibEntryType2Str (BibGenericEntry s) = '@':s

data BibItem =
  BibEntry {
      entryKey :: String
    , entryType :: BibEntryType
    -- | Publisher's address (usually just the city, but can be the full address for lesser-known publishers)
    , bibAddress :: String
    -- | An annotation for annotated bibliography styles (not typical)
    , bibAnnote :: String
    -- | The name(s) of the author(s) (in the case of more than one author, separated by and)
    , bibAuthor :: [PersonName]
    -- | The title of the book, if only part of it is being cited
    , bibBookTitle :: String
    -- | The chapter number
    , bibChapter :: String
    -- | The key of the cross-referenced entry
    , bibCrossRef :: String
    -- | The edition of a book, long form (such as "First" or "Second")
    , bibEdition :: String
    -- | The name(s) of the editor(s)
    , bibEditor :: [PersonName]
    -- | How it was published, if the publishing method is nonstandard
    , bibHowPublished :: String
    -- | The institution that was involved in the publishing, but not necessarily the publisher
    , bibInstitution :: String
    -- | The journal or magazine the work was published in
    , bibJournal :: String
    -- | A hidden field used for specifying or overriding the alphabetical order of entries (when the "author" and "editor" fields are missing). Note that this is very different from the key (mentioned just after this list) that is used to cite or cross-reference the entry.
    , bibKey :: String
    -- | The month of publication (or, if unpublished, the month of creation)
    , bibMonth :: String
    -- | Miscellaneous extra information
    , bibNote :: String
    -- | The "(issue) number" of a journal, magazine, or tech-report, if applicable. (Most publications have a "volume", but no "number" field.)
    , bibNumber :: String
    -- | The conference sponsor
    , bibOrganization :: String
    -- | Page numbers, separated either by commas or double-hyphens.
    , bibPages :: String
    -- | The publisher's name
    , bibPublisher :: String
    -- | The school where the thesis was written
    , bibSchool :: String
    -- | The series of books the book was published in (e.g. "The Hardy Boys" or "Lecture Notes in Computer Science")
    , bibSeries :: String
    -- | The title of the work
    , bibTitle :: String
    -- | The field overriding the default type of publication (e.g. "Research Note" for techreport, "{PhD} dissertation" for phdthesis, "Section" for inbook/incollection)
    , bibType :: String
    -- | The volume of a journal or multi-volume book
    , bibVolume :: String
    -- | The year of publication (or, if unpublished, the year of creation)
    , bibYear :: String
    -- | Non-standard tags
    , bibExtraTags :: [(String, String)]
    }
  -- | @COMMENT for comments not taken in regard by BibTeX.
  | BibComment String
  -- | `@STRING` defines abbreviations in the form of
  -- `@string { foo = "Mrs. Foo" }`
  -- which can later be used in a tag like this
  -- `author = foo # " and Mr. Bar"`
  | BibString { stringId    :: String
              , stringValue :: String
              }
  -- | @PREAMBLE defines how special text should be formatted.
  | BibPreamble String

data PersonName = PersonName { firstName :: String
                             , lastName  :: String
                             }

personNames2Str :: [PersonName] -> String
personNames2Str = intercalate " and " . map (\(PersonName fs lt) -> lt ++ ", " ++ fs)
