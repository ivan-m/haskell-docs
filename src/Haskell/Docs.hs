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
printDocForIdentInModule
  :: Maybe PackageName -- ^ Package.
  -> ModuleName        -- ^ Module name.
  -> Identifier        -- ^ Identifier.
  -> Ghc ()
printDocForIdentInModule pname mname name =
  do result <- search Nothing pname mname name
     case result of
       Left err ->
         error (show err)
       Right docs ->
         mapM_ printIdentDoc docs
