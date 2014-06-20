{-# LANGUAGE TupleSections #-}
-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).

module Haskell.Docs
  (module Haskell.Docs
  ,Identifier(..)
  ,PackageName(..))
  where

import Haskell.Docs.Formatting
import Haskell.Docs.Haddock
import Haskell.Docs.Types

import GHC hiding (verbosity)
import GhcMonad (liftIO)
import System.IO

-- -- | Print the documentation of a name in the given module.
searchAndPrintDoc
  :: Maybe PackageName -- ^ Package.
  -> Maybe ModuleName  -- ^ Module name.
  -> Identifier        -- ^ Identifier.
  -> Ghc ()
searchAndPrintDoc pname mname ident =
  do (result,printPkg,printModule) <- search
     case result of
       Left err ->
         error (show err)
       Right docs ->
         mapM_ (printIdentDoc printPkg printModule)
               docs
  where search =
          case (pname,mname) of
            (Just p,Just m) -> fmap (,False,False) (searchPackageModuleIdent Nothing p m ident)
            (Nothing,Just m) -> fmap (,True,False) (searchModuleIdent Nothing m ident)
            (Nothing,Nothing) -> fmap (,True,True) (searchIdent Nothing ident)
