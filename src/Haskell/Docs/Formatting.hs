{-# LANGUAGE CPP #-}

-- | Formatting of Haddock documentation.

module Haskell.Docs.Formatting where

import Haskell.Docs.Ghc
import Haskell.Docs.HaddockDoc
import Haskell.Docs.Types

import Control.Monad
import Data.List
import Documentation.Haddock
import GHC hiding (verbosity)
import GhcMonad (liftIO)

-- * Formatting

-- | Print docs as s-expressions.
printSexp :: [IdentDoc] -> Ghc ()
printSexp = mapM toSexp >=> liftIO . putStrLn . renderSexp . List

-- | Print an identifier' documentation.
printIdentDoc :: Bool -- ^ Print modules only?
              -> Bool -- ^ Print package?
              -> Bool -- ^ Print module?
              -> IdentDoc
              -> Ghc ()
printIdentDoc True _ _ idoc =
  do d <- getSessionDynFlags
     maybe (liftIO (putStrLn (showppr d (identDocModuleName idoc))))
           (\i -> liftIO (putStrLn (showppr d (moduleName (nameModule (getName i))))))
           (identDocIdent idoc)
printIdentDoc _ printPkg printModule idoc =
  do d <- getSessionDynFlags
     when printPkg
          (liftIO (putStrLn ("Package: " ++ showPackageName (identDocPackageName idoc))))
     when printModule
          (maybe (liftIO (putStrLn ("Module: " ++ showppr d (identDocModuleName idoc))))
                 (\i -> liftIO (putStrLn ("Module: " ++
                                          showppr d (moduleName (nameModule (getName i))))))
                 (identDocIdent idoc))
     case identDocIdent idoc of
       Nothing -> liftIO (putStrLn (unIdentifier (identDocIdentifier idoc)))
       Just i -> liftIO (putStrLn (showppr d i ++ " :: " ++ showppr d (idType i)))
     liftIO (putStrLn (formatDoc (identDocDocs idoc)))
     case identDocArgDocs idoc of
       Nothing -> return ()
       Just args -> liftIO (putStr (unlines (map (\(i,x) -> formatArg i x) args)))

-- | Format some documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . doc where

-- * Internal functions

-- | S-expression type.
data Sexp
  = Atom String
  | String String
  | List [Sexp]

-- | Render an s-expression to string.
renderSexp :: Sexp -> String
renderSexp (Atom string) = string
renderSexp (String string) = show string
renderSexp (List sexps) = "(" ++ intercalate " " (map renderSexp sexps) ++ ")"

-- | Convert docs to an s-expression.
toSexp :: IdentDoc -> Ghc Sexp
toSexp idoc =
  do d <- getSessionDynFlags
     return (List (concat (object d)))
  where
    object d =
      [[List [Atom "package",String (showPackageName (identDocPackageName idoc))]]
      ,[List [Atom "module",String (showppr d (moduleName (nameModule (getName i))))]
       |Just i <- [identDocIdent idoc]]
      ,[List [Atom "type",String (showppr d (idType i))]
       |Just i <- [identDocIdent idoc]]
      ,[List [Atom "arguments",List ((map (\(i,x) -> String (formatArg i x)) args))]
       |Just args <- [identDocArgDocs idoc]]
      ,[List [Atom "documentation",String (formatDoc (identDocDocs idoc))]]]

-- | Format an argument.
formatArg :: Show a => a -> Doc String -> String
formatArg i x = prefix ++
                indentAfter (length prefix) (formatDoc x)
  where prefix = show i ++ ": "

-- | Indent after the first line.
indentAfter :: Int -> String -> String
indentAfter i xs = intercalate "\n" (take 1 l ++ map (replicate (i-1) ' ' ++) (drop 1 l))
  where l = lines xs
