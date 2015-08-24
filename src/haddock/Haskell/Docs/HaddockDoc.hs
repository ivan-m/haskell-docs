{-# LANGUAGE CPP #-}
-- | Pre-haddock-api.

module Haskell.Docs.HaddockDoc where

import           Control.Arrow
import           Data.Char             (isSpace)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Documentation.Haddock
import           GHC                   hiding (verbosity)
import           Name

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

#if MIN_VERSION_haddock(2,10,0)
-- The header type is unexported, so this constructor is useless.
doc (DocIdentifier i) = i
doc (DocWarning d) = "Warning: " ++ doc d
#else
doc (DocPic pic) = pic
doc (DocIdentifier i) = intercalate "." i
#endif

#if MIN_VERSION_haddock(2,11,0)
doc (DocIdentifierUnchecked (mname,occname)) =
  moduleNameString mname ++ "." ++ occNameString occname
doc (DocPic pic) = show pic
#endif

#if MIN_VERSION_haddock(2,13,0)
doc (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
doc (DocProperty p) = "Property: " ++ p
#else
doc (DocURL url) = url
#endif

#if MIN_VERSION_haddock(2,14,0)
doc (DocBold d) = "**" ++ doc d ++ "**"
doc (DocHeader _) = ""
#endif

-- * Get documentation of parts of things

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
interfaceNameMap = M.fromList
                   . map (getOccString
                          ***
                          fmap getOccString
#if !(MIN_VERSION_haddock(2,10,0))
                            . maybe DocEmpty id . fst
#endif
                         )
                   . M.toList
                   . instDocMap

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceArgMap :: InstalledInterface -> Map String (Map Int (Doc Name))
interfaceArgMap = M.fromList
                  . map (
#if !(MIN_VERSION_haddock(2,10,0))
                          first getOccString
#else
                          getOccString *** const M.empty
#endif
                        )
                  . M.toList
                  . instDocMap

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
