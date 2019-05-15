{-# LANGUAGE CPP #-}

-- | Post-haddock-api.

module Haskell.Docs.HaddockDoc where

import           Control.Arrow
import           Data.Char
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Documentation.Haddock (Doc, DocH(..), Example(..),
                                        Hyperlink(..), InstalledInterface(..))
import           GHC                   (Name, moduleNameString)
import           Name                  (getOccString, occNameString)

#if MIN_VERSION_haddock_api(2,16,0)
import Documentation.Haddock.Types (_doc)
#endif

-- | Render the doc.
doc :: Doc String -> String
doc DocEmpty = ""
doc (DocAppend a b) = doc a ++ doc b
doc (DocString str) = normalize str
doc (DocParagraph p) = doc p ++ "\n"
doc (DocModule m) = m
doc (DocEmphasis e) = "*" ++ doc e ++ "*"
doc (DocMonospaced e) = "`" ++ doc e ++ "`"
doc (DocUnorderedList i) = unlines (map (("* " ++) . doc) i)
doc (DocOrderedList i) = unlines (zipWith (\j x -> show j ++ ". " ++ doc x) [1 :: Int ..] i)
doc (DocDefList xs) = unlines (map (\(i,x) -> doc i ++ ". " ++ doc x) xs)
doc (DocCodeBlock bl) = unlines (map ("    " ++) (lines (doc bl))) ++ "\n"
doc (DocAName name) = name
doc (DocExamples exs) = unlines (map formatExample exs)
#if MIN_VERSION_haddock_api (2,17,0)
doc (DocMathInline mth) = mth
doc (DocMathDisplay mth) = mth
#if MIN_VERSION_haddock_api (2,19,0)
doc (DocTable _) = ""
#endif
#endif

-- The header type is unexported, so this constructor is useless.
doc (DocIdentifier i) = i
doc (DocWarning d) = "Warning: " ++ doc d
doc (DocIdentifierUnchecked (mname,occname)) =
  moduleNameString mname ++ "." ++ occNameString occname
doc (DocPic pic) = show pic
doc (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
doc (DocProperty p) = "Property: " ++ p
doc (DocBold d) = "**" ++ doc d ++ "**"
doc (DocHeader _) = ""

-- * Get documentation of parts of things

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
interfaceNameMap = M.fromList
                   . map ( getOccString
                           ***
#if MIN_VERSION_haddock_api(2,16,0)
                           _doc .
#endif
                             fmap getOccString
                         )
                   . M.toList
                   . instDocMap

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceArgMap :: InstalledInterface -> Map String (Map Int (Doc Name))
interfaceArgMap = M.fromList
#if MIN_VERSION_haddock_api(2,16,0)
                  . map (getOccString *** fmap _doc)
#else
                  . map (first getOccString)
#endif
                  . M.toList
                  . instArgMap

-- | Strip redundant whitespace.
normalize :: [Char] -> [Char]
normalize = go where
  go (' ':' ':cs) = go (' ':cs)
  go (c:cs)       = c : go cs
  go []           = []

-- | Trim either side of a string.
trim :: [Char] -> [Char]
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Format an example to plain text.
formatExample :: Example -> String
formatExample (Example expression result) =
  "    > " ++ expression ++
  unlines (map ("    " ++) result)
