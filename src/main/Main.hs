{-# OPTIONS -Wall #-}

-- | Main command-line interface.

module Main where

import Haskell.Docs
import Haskell.Docs.Ghc

import System.Environment

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     withInitializedPackages
       []
       (case args of
          [name] ->
            searchAndPrintDoc Nothing Nothing (Identifier name)
          [mname,name,pname] ->
            searchAndPrintDoc (Just (PackageName pname))
                              (Just (makeModuleName mname))
                              (Identifier name)
          [mname,name] ->
            searchAndPrintDoc Nothing
                              (Just (makeModuleName mname))
                              (Identifier name)
          _ -> error "<module-name> <ident> [<package-name>] | <ident>")
