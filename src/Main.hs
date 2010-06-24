-----------------------------------------------------------------------------
-- Module      : Main
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 06/23/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Main where

import Text.XML.Prettify
import System.Environment


main = do
  args <- getArgs
  let infile = args !! 0
  input <- readFile infile
  let tags = inputToTags input
  printAllTags tags


  
