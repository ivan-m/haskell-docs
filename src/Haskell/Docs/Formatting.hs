{-# LANGUAGE CPP #-}

-- | Formatting of Haddock documentation.

module Haskell.Docs.Formatting where

import Haskell.Docs.Types
import Haskell.Docs.Ghc

import Data.Char
import Data.List
import Documentation.Haddock
import GHC hiding (verbosity)
import GhcMonad (liftIO)
import Name

-- * Formatting

-- | Print an identifier' documentation.
printIdentDoc :: IdentDoc -> Ghc ()
printIdentDoc idoc =
  do d <- getSessionDynFlags
     liftIO (putStrLn ("Package: " ++ showPackageName (identDocPackageName idoc)))
     case identDocIdent idoc of
       Nothing -> return ()
       Just i -> liftIO (putStrLn (showppr d i ++ " :: " ++ showppr d (idType i)))
     liftIO (putStrLn (formatDoc (identDocDocs idoc)))
     case identDocArgDocs idoc of
       Nothing -> return ()
       Just args -> liftIO (putStr (unlines (map (\(i,x) -> formatArg i x) args)))

-- | Format some documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . doc where

-- * Internal functions

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

-- | Format an argument.
formatArg :: Show a => a -> Doc String -> String
formatArg i x = prefix ++
                indentAfter (length prefix) (formatDoc x)
  where prefix = show i ++ ": "

-- | Indent after the first line.
indentAfter :: Int -> String -> String
indentAfter i xs = intercalate "\n" (take 1 l ++ map (replicate (i-1) ' ' ++) (drop 1 l))
  where l = lines xs
