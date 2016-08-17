{-# LANGUAGE CPP #-}

-- | Cabal.

module Haskell.Docs.Cabal where

import Haskell.Docs.Ghc

import Data.Char                         (isSpace)
import Data.List
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import DynFlags
import GHC
import Module

#if __GLASGOW_HASKELL__ >= 710
import PackageConfig hiding (InstalledPackageInfo(..))
#else
import PackageConfig
#endif

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
     return (fromMaybe [] (pkgDB flags))

pkgDB :: DynFlags -> Maybe [PackageConfig]
#if __GLASGOW_HASKELL__ >= 800
pkgDB = fmap (concatMap snd) . pkgDatabase
#else
pkgDB = pkgDatabase
#endif

-- | Version-portable version of allPackagesByName.
#if MIN_VERSION_Cabal (1,22,0)
packagesByName :: InstalledPackageIndex -> [[InstalledPackageInfo]]
#else
packagesByName :: PackageIndex -> [[InstalledPackageInfo]]
#endif
#if MIN_VERSION_Cabal(1,16,0)
packagesByName = map snd . allPackagesByName
#else
packagesByName = allPackagesByName
#endif

-- * Internal modules

-- | Convert a Cabal module name to a GHC module name.
convModule :: Distribution.ModuleName.ModuleName -> Module.ModuleName
convModule = makeModuleName . intercalate "." . components
