{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Documentation.Haddock.Docs
import GHC (mkModuleName)
import System.Environment
import Options.Applicative

main = do
    (mname, name, pname, ghcopts) <- execParser opts
    withInitializedPackages ghcopts $ \d -> void $
        printDocumentation d name (mkModuleName mname) pname Nothing
 where
   opts = info (helper <*> p) fullDesc
   p = (\a b c d -> (a, b, c, d)) <$> pModuleName <*> pName <*> (optional pPackageName) <*> pGhcOptions

   pModuleName  = argument str (metavar "<modulename>")
   pName        = argument str (metavar "<name>")
   pPackageName = argument str (metavar "<package name>")
   pGhcOptions  = many $ strOption (long "ghc-options" <> short 'g')
