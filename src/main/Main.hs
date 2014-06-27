{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- | Main command-line interface.

module Main where

import           Haskell.Docs
import           Haskell.Docs.Ghc
import           Haskell.Docs.Types

import           Control.Exception
import           Control.Exception (IOException)
import qualified Control.Exception as E
import           Data.Monoid
import           Data.Text (unpack)
import           Data.Typeable
import           GHC
import           GhcMonad
import           System.Environment
import           System.Exit
import           System.IO

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     app args

-- | Do the printing.
app :: [String] -> IO ()
app (extract -> x@(gs,ms,as,ss)) =
  do if ms
        then case as of
               [name]     -> searchAndPrintModules (Identifier name)
               [_,name,_] -> searchAndPrintModules (Identifier name)
               [_,name]   -> searchAndPrintModules (Identifier name)
        else withInitializedPackages
               gs
               (\packages ->
                  catchErrors
                    (case as of
                       [name] ->
                         searchAndPrintDoc packages
                                           ms
                                           ss
                                           Nothing
                                           Nothing
                                           (Identifier name)
                       [mname,name,pname] ->
                         searchAndPrintDoc packages
                                           ms
                                           ss
                                           (Just (PackageName pname))
                                           (Just (makeModuleName mname))
                                           (Identifier name)
                       [mname,name] ->
                         searchAndPrintDoc packages
                                           ms
                                           ss
                                           Nothing
                                           (Just (makeModuleName mname))
                                           (Identifier name)
                       _ -> bail "<module-name> <ident> [<package-name>] | <ident>\n\
                                 \\n\
                                 \Options: --g <ghc option> Specify GHC options.\n\
                                 \         --sexp           Output s-expressions.\n\
                                 \         --modules        Only output modules." ))

-- | Extract arguments.
extract :: [String] -> ([String],Bool,[String],Bool)
extract = go ([],False,[],False)
  where
    go (gs,ms,as,ss) ("-g":arg:ys)    = go (arg:gs,ms,as,ss) ys
    go (gs,ms,as,ss) ("--modules":ys) = go (gs,True,as,ss) ys
    go (gs,ms,as,ss) ("--sexp":ys)    = go (gs,ms,as,True) ys
    go (gs,ms,as,ss) (y:ys)           = go (gs,ms,y:as,ss) ys
    go (gs,ms,as,ss) []               = (gs,ms,reverse as,ss)

-- | Catch errors and print 'em out.
catchErrors :: Ghc () -> Ghc ()
catchErrors m =
  gcatch
    m
    (\(SomeException e) ->
       case cast e of
         Just ex ->
           do bail (printEx ex)
              liftIO exitFailure
         Nothing ->
           bail ("Exception: " ++ show e ++ " :: " ++ show (typeOf e) ++ ""))

-- | Print an error and bail out.
bail :: String -> Ghc ()
bail e =
  liftIO (hPutStrLn stderr e)

-- | Print an exception for humans.
printEx :: DocsException -> String
printEx e =
  case e of
    NoFindModule -> "Couldn't find any packages with that module."
    PackageLookupFailed t -> "Couldn't lookup the right package: " <> unpack t
    NoModulePackageCombo -> "Couldn't match a module with that package."
    NoInterfaceFiles -> "No interface files to search through! \
                        \Maybe you need to generate documentation for the package?"
    NoParseInterfaceFiles reasons -> "Couldn't parse interface files: " ++
                                     unlines (map printEx reasons)
    NoFindNameInExports -> "Couldn't find that name in an export list."
    NoFindNameInInterface -> "Couldn't find name in interface."
    NoReadInterfaceFile _ -> "Couldn't read the interface file."
  where (<>) = mappend
