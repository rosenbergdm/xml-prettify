{-# LANGUAGE DeriveDataTypeable #-}
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

module Main where

import Text.XML.Prettify
import System.Console.CmdArgs
import System.Posix
import Control.Monad

data XmlPrettify = XmlPrettify
  { input_files :: [String] 
  } deriving (Show, Data, Typeable)

xmlprettify = mode $ XmlPrettify {  input_files = def &= args }

modes = [xmlprettify]

main = do
  prettify <- cmdArgs "xml-prettify v0.0.3, (c) David M. Rosenberg 2010" [xmlprettify]
  let infiles = input_files prettify
  goodFile <- liftM (all (== True)) $ mapM fileExist infiles 
  result <- case (goodFile, infiles) of
                 (_, [])      ->showHelp
                 (True, _)    ->prettyPrint infiles
                 (False, _)   ->showHelp
  putStrLn result
  return ()
  

prettyPrint :: [String] -> IO String
prettyPrint [] = do
  putStr ""
  return ""
prettyPrint (x:xs) = do
  pp <- prettyP x
  res <- prettyPrint xs
  return $ concat [pp, "\n", res]
  
prettyP :: String -> IO String
prettyP input_file = do
  input <- readFile input_file
  let tags = inputToTags input
      z    = printAllTags tags
  return z

showHelp = do
  helptext <- cmdArgsHelp "xml-prettify v0.0.3, (c) David M. Rosenberg 2010" [xmlprettify] Text
  return helptext
