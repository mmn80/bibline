{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

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

import           Control.Monad.Except
import           Data.Char            (isAlphaNum, isSpace)
import           Data.Text            (Text, singleton, unpack)
import qualified Data.Text            as T
import           Pipes
import           Pipes.Parse
import           Text.Bibline.Types

data BibParseResult = BibParseResultOk | BibSyntaxError String
  deriving (Eq)

instance Show BibParseResult where
  show BibParseResultOk = "Parsing finished with success."
  show (BibSyntaxError msg) = "Parse error: " ++ msg

biblined :: Monad m => Producer Text m r ->
                       Producer BibItem m (BibParseResult, Producer Text m r)
biblined = parsed itemParser

type BibParser m r = ExceptT BibParseResult (StateT (Producer Text m r) m)

drawWord :: Monad m => BibParser m r (Text, Text)
drawWord = go (T.empty, T.empty)
  where go (spc, wrd) = lift draw >>= \case
          Nothing  -> return (spc, wrd)
          Just txt ->
            let (s, t) = if T.null wrd then let (s', t') = T.span isSpace txt in
                                            (spc `T.append` s', t')
                         else (spc, txt)
                (w, r) = T.span (\c -> isAlphaNum c || c `elem` "-_.") t in
            if | T.null r ->
                   go (s, wrd `T.append` w)
               | T.null w && T.null wrd ->
                   let Just (c, r') = T.uncons r in
                   lift (unDraw r') >> return (s, singleton c)
               | otherwise ->
                   lift (unDraw r) >> return (s, wrd `T.append` w)

drawWord' :: Monad m => BibParser m r Text
drawWord' = snd <$> drawWord

drawToken :: Monad m => BibParser m r (Text, Text)
drawToken = do
  (s, w) <- drawWord
  if w == singleton '\\' then do
    (s', w') <- drawWord
    if T.null s' then return (w `T.append` w', s)
    else lift (unDraw $ s' `T.append` w') >> return (w, s)
  else return (w, s)

expect :: Monad m => Text -> BibParser m r ()
expect txt = do
  w <- drawWord'
  unless (w == txt) $
    throwError $ BibSyntaxError $ "Expecting \"" ++ unpack txt
      ++ "\" but got " ++ if T.null w then "[END OF INPUT]"
                          else "\"" ++ unpack w ++ "\""

expectChar :: Monad m => Char -> BibParser m r ()
expectChar = expect . singleton

drawBlock :: Monad m => Bool -> BibParser m r Text
drawBlock isTag = go T.empty []
  where go acc ctx = do
          (w, s) <- drawToken
          if (not (T.null s) || w == singleton ',' || w == singleton '}')
             && not (T.null acc) && null ctx
          then lift (unDraw $ s `T.append` w) >> return acc
          else let ctx' = case T.uncons w of
                            Nothing      -> ctx
                            Just (c, wr) -> if T.null wr then updCtx c ctx
                                            else ctx
                   acc' = acc `T.append` s `T.append` w in
               if null ctx' && not (T.null acc) then return
                 (if isTag then acc'
                  else T.strip $ T.dropEnd 1 $ T.drop 1 $ T.strip acc')
               else go acc' ctx'
        updCtx '{' ctx = '{':ctx
        updCtx '"' ctx = case ctx of
          '"':ctx' -> ctx'
          _:_      -> '"':ctx
          []       -> if isTag then "\"" else []
        updCtx '}' ctx = case ctx of
          '{':ctx' -> ctx'
          _        -> ctx
        updCtx _ ctx = ctx

drawTag :: Monad m => BibParser m r (Text, Text)
drawTag = (,) <$> drawWord' <*> do
  expectChar '='
  blk <- drawBlock True
  (s, w) <- drawWord
  unless (w == T.singleton ',') $ lift $ unDraw $ s `T.append` w
  return $ T.strip blk

drawString :: Monad m => BibParser m r BibItem
drawString = do
  expectChar '{'
  (t, v) <- drawTag
  expectChar '}'
  return $ BibString t v

itemParser :: Monad m => Parser Text m (Either BibParseResult BibItem)
itemParser = runExceptT $ do
  (_, at) <- drawWord
  when (T.null at) $ throwError BibParseResultOk
  lift $ unDraw at
  expectChar '@'
  (s, w) <- drawWord
  let ty = unpack $ T.toLower w
  if | not (T.null s)   -> throwError $ BibSyntaxError $
       "Expecting identifier after @ but got \"" ++ unpack s ++ "\""
     | ty == "preamble" -> BibPreamble <$> drawBlock False
     | ty == "comment"  -> BibComment <$> drawBlock False
     | ty == "string"   -> drawString
     | otherwise -> do
       expectChar '{'
       (_, ek) <- drawWord
       let err = "Citation key expected but got \"" ++ T.unpack ek ++ "\""
       initE <- case T.uncons ek of
         Nothing -> throwError $ BibSyntaxError err
         Just (c, _) -> if isAlphaNum c
                        then return emptyEntry { entryType = read ty
                                               , entryKey  = ek }
                        else throwError $ BibSyntaxError err
       expectChar ','
       goTag initE
   where goTag it = do
           (s, w) <- drawWord
           if w == singleton '}' then return it
           else do
             lift $ unDraw $ s `T.append` w
             (t, v) <- drawTag
             goTag $ case unpack (T.toLower t) of
               "address"      -> it { bibAddress = v }
               "annote"       -> it { bibAnnote = v }
               "author"       -> it { bibAuthor = parsePersonName v }
               "booktitle"    -> it { bibBookTitle = v }
               "chapter"      -> it { bibChapter = v }
               "crossref"     -> it { bibCrossRef = v }
               "edition"      -> it { bibEdition = v }
               "editor"       -> it { bibEditor = parsePersonName v }
               "howpublished" -> it { bibHowPublished = v }
               "institution"  -> it { bibInstitution = v }
               "journal"      -> it { bibJournal = v }
               "key"          -> it { bibKey = v }
               "month"        -> it { bibMonth = v }
               "note"         -> it { bibNote = v }
               "number"       -> it { bibNumber = v }
               "organization" -> it { bibOrganization = v }
               "pages"        -> it { bibPages = v }
               "publisher"    -> it { bibPublisher = v }
               "school"       -> it { bibSchool = v }
               "series"       -> it { bibSeries = v }
               "title"        -> it { bibTitle = v }
               "type"         -> it { bibType = v }
               "volume"       -> it { bibVolume = v }
               "year"         -> it { bibYear = v }
               "file"         -> it { bibFile = v }
               "keywords"     -> it { bibKeywords = parseKeywords v }
               _              -> it { bibExtraTags = bibExtraTags it ++ [(t, v)] }

parseKeywords :: Text -> [Text]
parseKeywords = fmap T.strip . T.split (== ',')

parsePersonName :: Text -> [PersonName]
parsePersonName = map (mkp . map T.strip . T.splitOn (T.pack ","))
    . T.splitOn (T.pack " and ") . stripParens . T.strip
  where mkp = \case
          [] -> p0
          k1:k1s -> case k1s of
            [] -> case T.words k1 of
                    []     -> p0
                    n1:n1s -> let p1 = p0 { firstName = n1 } in case n1s of
                      []     -> p1
                      n2:n2s -> case n2s of
                        []     -> p1 { lastName = n2 }
                        n3:n3s -> case n3s of
                          [] -> if von n2 then p1 { lastName = n2
                                  `T.append` T.singleton ' ' `T.append` n3 }
                                else p1 { lastName = n3, middleName = n2 }
                          _  -> p1 { lastName = n3, middleName = n2
                                   , nameSuffix = T.unwords n3s }
            k2:k2s ->
              let p1 = p0 { lastName = k1 }
                  f k p = case T.words k of
                         []     -> p
                         k':k's -> let p' = p { firstName = k' } in case k's of
                           [] -> p'
                           _  -> p' { middleName = T.unwords k's } in
              case k2s of
                [] -> f k2 p1
                _  -> f (T.unwords k2s) $ p1 { nameSuffix = k2 }
        p0 = PersonName T.empty T.empty T.empty T.empty
        von s = s == T.pack "von" || s == T.pack "van" || s == T.pack "der"
