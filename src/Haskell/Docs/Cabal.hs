{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cabal.

module Haskell.Docs.Cabal where

import Haskell.Docs.Ghc

import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import DynFlags
import GHC
import Module
import PackageConfig

-- * Cabal

getGhcOpsPackageDB :: [String] -> [PackageDB]
getGhcOpsPackageDB gs = map (SpecificPackageDB . trim) pkgDBOps
    where
        pkgDBOps = filter ("--package-db" `isPrefixOf`) gs
        trim = (drop 1) . snd . (break isSpace)

-- | Get all installed packages, filtering out the given package.
getAllPackages :: [String] -> Ghc [PackageConfig.PackageConfig]
getAllPackages _gs =
  do flags <- getSessionDynFlags
     return (fromMaybe [] (pkgDatabase flags))

-- | Version-portable version of allPackagesByName.
packagesByName :: PackageIndex -> [[InstalledPackageInfo]]
#if MIN_VERSION_Cabal(1,16,0)
packagesByName = map snd . allPackagesByName
#else
packagesByName = allPackagesByName
#endif

-- * Internal modules

-- | Convert a Cabal module name to a GHC module name.
convModule :: Distribution.ModuleName.ModuleName -> Module.ModuleName
convModule = makeModuleName . intercalate "." . components

-- | Because no Functor instance is available.
imap :: (a -> m) -> InstalledPackageInfo_ a -> InstalledPackageInfo_ m
imap f i@(InstalledPackageInfo{..}) =
  i { exposedModules = map f exposedModules
    , hiddenModules = map f hiddenModules }
