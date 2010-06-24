-----------------------------------------------------------------------------
-- Module      : XmlPrettify
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

module Text.XML.Prettify 
  ( TagType (..)
  , XmlTag (..)
  , inputToTags
  , lexOneTag
  , printTag
  , printTags
  , printAllTags
  ) where


data TagType = Inc | Dec | Standalone
  deriving (Read, Ord, Show, Eq, Enum)


data XmlTag = XmlTag
  { content :: String
  , tagtype :: TagType
  } deriving (Read, Ord, Eq, Show)


inputToTags :: String -> [XmlTag]
inputToTags [] = []
inputToTags "" = []
inputToTags st = 
  let (xtag, st') = lexOne st
  in (concat [[xtag], inputToTags st'])

lexOne :: String -> (XmlTag, String)
lexOne inp =
  let nextS   = dropWhile (\z -> z `elem` " \t\r\n") inp
      nextC   = head $ concat [nextS, " "]
      result  = case (nextC == '<', nextC == ' ') of
                      (_, True)  -> (XmlTag "" Standalone, "")
                      (True, _)  -> lexOneTag inp
                      (False, _) -> lexNonTagged inp
  in result

lexNonTagged :: String -> (XmlTag, String)
lexNonTagged inp = 
  let inp'       = dropWhile (\z -> z `elem` " \t\r\n") inp
      (con, rem) = span (\z -> z `notElem` " \n\r<") inp'
      xtag       = Standalone
  in (XmlTag con xtag, rem)


lexOneTag :: String -> (XmlTag, String)
lexOneTag inp = 
  let inp'       = dropWhile (/= '<') inp
      (con, rem) = span (/= '>') inp'
      contnt     = concat [con, [head rem]]
      res        = tail rem
      xtag       = case (head $ drop 1 contnt, head $ drop 1 $ reverse contnt) of
                     ('/', _) -> Dec
                     (_, '/') -> Standalone
                     ('!', _) -> Standalone
                     ('?', _) -> Standalone
                     (_, _)   -> Inc
  in (XmlTag contnt xtag, res)


printTag :: Int -> XmlTag -> IO Int
printTag ident tag = do
  let ident1    = case (tagtype tag) of
                    Dec    -> ident -1
                    _      -> ident
  let outstring = (replicate (ident1 * 2) ' ') ++ (content tag)
      ident2    = case (tagtype tag) of 
                    Inc    -> ident + 1
                    Dec    -> ident - 1
                    _      -> ident
  putStrLn outstring
  return ident2

printAllTags :: [XmlTag] -> IO ()
printAllTags tgs = printTags tgs 0

printTags :: [XmlTag] -> Int -> IO ()
printTags [] ident = do
  putStrLn ""
  return ()
printTags (tag:tags) ident = do
  ident' <- printTag ident tag
  res'   <- printTags tags ident'
  return res'

