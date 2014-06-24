{-# LANGUAGE DeriveDataTypeable #-}

-- | All types.

module Haskell.Docs.Types
  (IdentDoc(..)
  ,DocsException(..)
  ,Identifier(..)
  ,PackageName(..))
  where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Documentation.Haddock (Doc)
import GHC (Id)
import Module (ModuleName)
import PackageConfig (PackageIdentifier)

-- | An identifier.
newtype Identifier = Identifier {unIdentifier :: String}
  deriving (Show,Eq)

-- | An package name.
newtype PackageName = PackageName String
  deriving (Show,Eq)

-- | Identier documentation along with argument docs and identifiers.
data IdentDoc = IdentDoc
  { identDocPackageName :: !PackageIdentifier
  , identDocIdentifier  :: !Identifier
  , identDocModuleName  :: !ModuleName
  , identDocDocs        :: !(Doc String)
  , identDocIdent       :: !(Maybe Id)
  , identDocArgDocs     :: !(Maybe [(Int, Doc String)])
  }

instance Eq IdentDoc where
  a == b = identDocIdent a == identDocIdent b

-- | An exception when doing lookups.
data DocsException
  = NoFindModule
  | NoModulePackageCombo
  | NoInterfaceFiles
  | NoParseInterfaceFiles [DocsException]
  | NoFindNameInExports
  | NoFindNameInInterface
  | NoReadInterfaceFile String
  deriving (Typeable,Show)
instance Exception DocsException
