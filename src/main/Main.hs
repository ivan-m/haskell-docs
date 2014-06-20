-- | Main command-line interface.

module Main where

import Haskell.Docs
import Haskell.Docs.Types
import Haskell.Docs.Ghc

import qualified Control.Exception as E
import System.Environment

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     E.catch
       (app args)
       (error . printEx)

-- | Do the printing.
app :: [String] -> IO ()
app args =
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

-- | Print an exception for humans.
printEx :: DocsException -> String
printEx e =
  case e of
    NoFindModule -> "Couldn't find any packages with that module."
    NoModulePackageCombo -> "Couldn't match a module with that package."
    NoInterfaceFiles -> "No interface files to search through! \
                        \Maybe you need to generate documentation for the package?"
    NoParseInterfaceFiles reasons -> "Couldn't parse interface files: " ++
                                     unlines (map printEx reasons)
    NoFindNameInExports -> "Couldn't find that name in an export list."
    NoFindNameInInterface -> "Couldn't find name in interface."
    NoReadInterfaceFile _ -> "Couldn't read the interface file."
