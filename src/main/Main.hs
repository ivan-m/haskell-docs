{-# OPTIONS -Wall #-}

-- | Main command-line interface.

module Main where

import Control.Monad
import Haskell.Docs
import Options.Applicative

-- | Main entry point.
main :: IO ()
main = do
  (mname, name, pname, ghcopts) <- execParser opts
  withInitializedPackages ghcopts $ \d -> void $
      printDocumentation d name (makeModuleName mname) pname Nothing
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
