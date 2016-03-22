-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : MIT
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Bibtex console utility.
-----------------------------------------------------------------------------

module Main (main) where

import           Control.Monad (unless)
import           Data.Text     (pack)
import qualified Data.Text.IO  as T
import           Pipes
import           Pipes.Parse   (runStateT)
import qualified Pipes.Prelude as P
import qualified Pipes.Text    as PT
import qualified Pipes.Text.IO as PT
import           System.IO     (hPrint, hPutStrLn, stderr)
import           Text.Bibline

main = do
  (r, p) <- runEffect $
              for (biblined PT.stdin >-> P.map (pack . show)) $
                liftIO . T.putStr
  hPrint stderr r
  (used, p') <- runStateT PT.isEndOfChars p
  unless used $ do
    hPutStrLn stderr "Unused input:"
    runEffect $ for p' (liftIO . T.hPutStr stderr)
