{-# OPTIONS -Wall #-}

module Main where

import Control.Monad
import Documentation.Haddock.Docs
import GHC (mkModuleName)
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mname,name,pname] -> withInitializedPackages $ \d -> void $
                            printDocumentation d name (mkModuleName mname) (Just pname) Nothing
    [mname,name] -> withInitializedPackages $ \d -> void $
                      printDocumentation d name (mkModuleName mname) Nothing Nothing
    _ -> error "arguments: <modulename> <name> [<package name>]"
