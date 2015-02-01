{-# LANGUAGE CPP #-}

-- | Post-haddock-api.

module Haskell.Docs.HaddockDoc where

import           Control.Arrow
import           Control.Exception (try,IOException)
import           Control.Monad
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Documentation.Haddock
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GHC hiding (verbosity)
import           GhcMonad (liftIO)
import           GhcMonad (liftIO)
import           Haskell.Docs.Cabal
import           Haskell.Docs.Ghc
import           Haskell.Docs.Ghc
import           Haskell.Docs.Types
import           Haskell.Docs.Types
import           Name
import           Name
import           PackageConfig
import           Packages

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

#if MIN_VERSION_haddock_api(2,10,0)
-- The header type is unexported, so this constructor is useless.
doc (DocIdentifier i) = i
doc (DocWarning d) = "Warning: " ++ doc d
#else
doc (DocPic pic) = pic
doc (DocIdentifier i) = intercalate "." i
#endif

#if MIN_VERSION_haddock_api(2,11,0)
doc (DocIdentifierUnchecked (mname,occname)) =
  moduleNameString mname ++ "." ++ occNameString occname
doc (DocPic pic) = show pic
#endif

#if MIN_VERSION_haddock_api(2,13,0)
doc (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
doc (DocProperty p) = "Property: " ++ p
#else
doc (DocURL url) = url
#endif

#if MIN_VERSION_haddock_api(2,14,0)
doc (DocBold d) = "**" ++ doc d ++ "**"
doc (DocHeader _) = ""
#endif

-- * Get documentation of parts of things

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
#if MIN_VERSION_haddock_api(2,10,0)
interfaceNameMap iface =
  M.fromList (map (second (fmap getOccString) . first getOccString)
             (M.toList (instDocMap iface)))
#else
interfaceNameMap iface =
  M.fromList (map (second (fmap getOccString . maybe DocEmpty id . fst) . first getOccString)
             (M.toList (instDocMap iface)))
#endif

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceArgMap :: InstalledInterface -> Map String (Map Int (Doc Name))
#if MIN_VERSION_haddock_api(2,10,0)
interfaceArgMap iface =
  M.fromList (map (first getOccString) (M.toList (instArgMap iface)))
#else
interfaceArgMap iface = M.fromList (map (second (const M.empty) . first getOccString)
                                        (M.toList (instDocMap iface)))
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
