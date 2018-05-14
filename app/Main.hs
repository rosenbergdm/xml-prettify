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
--    Main command-line utility.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Monad                   (forM_, when, (>=>))
import           System.Console.CmdArgs.Implicit
import           Text.XML.Prettify

data Options = Options
  { separator :: Bool
  , files     :: [FilePath]
  }
  deriving (Data, Eq, Show, Typeable)

options :: Options
options = Options
  { separator = def
    &= typ "BOOL"
    &= help "Print a line of hypens before output of each file."
  , files = def
    &= args
    &= typ "XML files"
  }
  &= summary "xml-prettify v0.0.3, (c) David M. Rosenberg 2010"
  &= details [ "Pretty prints each file given on the command line."
             , "Optionally separates them with a line of hypens."
             , ""
             , "If no args are given, takes input from stdin."
             ]

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode options

main = cmdArgs options >>= \opts ->
  case files opts of
    [] -> getContents >>= prettyPrint opts
    fs -> prettyPrintFiles opts fs

prettyPrintFiles :: Foldable t => Options -> t FilePath -> IO ()
prettyPrintFiles opts fs =
  forM_ fs $ readFile >=> prettyPrint opts

prettyPrint :: Options -> String -> IO ()
prettyPrint opts input = do
  let tags = inputToTags input
      z    = printAllTags tags
  when (separator opts) $ putStrLn "--------------------------------------------------"
  forM_ (lines z) putStrLn
