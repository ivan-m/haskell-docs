{-# LANGUAGE DeriveDataTypeable #-}

-- | All types.

module Haskell.Docs.Types
  (IdentDoc(..)
  ,DocsException(..))
  where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Documentation.Haddock (Doc)
import GHC (Id)
import PackageConfig (PackageIdentifier)

-- | Identier documentation along with argument docs and identifiers.
data IdentDoc = IdentDoc
  { identDocPackageName :: !PackageIdentifier
  , identDocDocs        :: !(Doc String)
  , identDocIdent       :: !(Maybe Id)
  , identDocArgDocs     :: !(Maybe [(Int, Doc String)])
  }

-- | An exception when doing lookups.
data DocsException
  = Couldn'tFindModule
  deriving (Typeable,Show)
instance Exception DocsException
