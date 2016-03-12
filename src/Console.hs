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

import Pipes
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Text.IO as Text
import Text.Bibline

main = runEffect $ (biblined Text.stdin
         >-> Pipes.map bibItem2Text >> return ())
         >-> Text.stdout
