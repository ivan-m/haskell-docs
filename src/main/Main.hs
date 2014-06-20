{-# OPTIONS -Wall #-}

-- | Main command-line interface.

module Main where

import Haskell.Docs
import Haskell.Docs.Ghc

import Options.Applicative

-- | Main entry point.
main :: IO ()
main =
  do (mname, name, pname, ghcopts) <- execParser opts
     withInitializedPackages
       ghcopts
       (printDocForIdentInModule (fmap PackageName pname)
                                 (makeModuleName mname)
                                 (Identifier name))
 where
   opts = info (helper <*> p) fullDesc
   p = (\a b c d -> (a, b, c, d))
       <$> pModuleName
       <*> pName
       <*> (optional pPackageName)
       <*> pGhcOptions
   pModuleName  = argument str (metavar "<modulename>")
   pName        = argument str (metavar "<name>")
   pPackageName = argument str (metavar "<package name>")
   pGhcOptions  = many $ strOption (long "ghc-options" `mappend` short 'g')
