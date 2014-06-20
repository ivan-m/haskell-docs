{-# LANGUAGE RecordWildCards #-}

-- | Cabal.

module Haskell.Docs.Cabal where

import Haskell.Docs.Ghc

import Data.List
import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Verbosity
import Module
import PackageConfig

-- * Cabal

-- | Get all installed packages.
getAllPackages :: IO [PackageConfig]
getAllPackages =
  do config <- configureAllKnownPrograms
                 normal
                 (addKnownPrograms [ghcProgram,ghcPkgProgram]
                                   emptyProgramConfiguration)
     index <- getInstalledPackages
                normal
                [GlobalPackageDB,UserPackageDB]
                config
     return (map (imap convModule)
                 (concat (allPackagesByName index)))

-- * Internal modules

-- | Convert a Cabal module name to a GHC module name.
convModule :: Distribution.ModuleName.ModuleName -> Module.ModuleName
convModule = makeModuleName . intercalate "." . components

-- | Because no Functor instance is available.
imap :: (a -> m) -> InstalledPackageInfo_ a -> InstalledPackageInfo_ m
imap f i@(InstalledPackageInfo{..}) =
  i { exposedModules = map f exposedModules
    , hiddenModules = map f hiddenModules }
