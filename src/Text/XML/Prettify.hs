-----------------------------------------------------------------------------
-- Module      : Text.XML.Prettify
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
  , lexOne
  , lexNonTagged
  ) where


data TagType = Inc | Dec | Standalone
  deriving (Read, Ord, Show, Eq, Enum)


data XmlTag = XmlTag
  { content :: String
  , tagtype :: TagType
  } deriving (Read, Ord, Eq, Show)


inputToTags :: String -> [XmlTag]
inputToTags "" = []
inputToTags st =
  let (xtag, st') = lexOne st
  in xtag : inputToTags st'

lexOne :: String -> (XmlTag, String)
lexOne inp =
  let nextS   = dropWhile (`elem` " \t\r\n") inp
      nextC   = head $ nextS ++ " "
      result  = case (nextC == '<', nextC == ' ') of
                      (_, True)  -> (XmlTag "" Standalone, "")
                      (True, _)  -> lexOneTag inp
                      (False, _) -> lexNonTagged inp
  in result

lexNonTagged :: String -> (XmlTag, String)
lexNonTagged inp =
  let inp'       = dropWhile (`elem` " \t\r\n") inp
      (con, rem) = span (`notElem` " \n\r<") inp'
      xtag       = Standalone
  in (XmlTag con xtag, rem)


lexOneTag :: String -> (XmlTag, String)
lexOneTag inp =
  let inp'       = dropWhile (/= '<') inp
      (con, rem) = span (/= '>') inp'
      contnt     = con ++ [head rem]
      res        = tail rem
      xtag       = case (contnt !! 1, reverse contnt !! 1) of
                     ('/', _) -> Dec
                     (_, '/') -> Standalone
                     ('!', _) -> Standalone
                     ('?', _) -> Standalone
                     (_, _)   -> Inc
  in (XmlTag contnt xtag, res)


printTag :: Int -> XmlTag -> (String, Int)
printTag ident tag =
  let ident1    = case tagtype tag of
                    Dec -> ident -1
                    _   -> ident
      outstring = replicate (ident1 * 2) ' ' ++ content tag
      ident2    = case tagtype tag of
                    Inc -> ident + 1
                    Dec -> ident - 1
                    _   -> ident
  in (outstring, ident2)

printAllTags :: [XmlTag] -> String
printAllTags tgs = printTags tgs 0

printTags :: [XmlTag] -> Int -> String
printTags [] ident = []
printTags (tag:tags) ident =
  let (txt, ident')  = printTag ident tag
  in concat [txt, "\n", printTags tags ident']

