{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- | Haddock compatibilty layer and query functions.

module Haskell.Docs.Haddock where

import           Control.Arrow
import           Data.Map (Map)
import qualified Data.Map as M
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           Name
import           PackageConfig
import           Packages

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
#if MIN_VERSION_haddock(2,10,0)
interfaceNameMap iface =
  M.fromList (map (second (fmap getOccString) . first getOccString)
             (M.toList (instDocMap iface)))
#else
interfaceNameMap iface =
  M.fromList (map (second (fmap getOccString . maybe DocEmpty id . fst) . first getOccString)
             (M.toList (instDocMap iface)))
#endif

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceArgMap :: InstalledInterface -> Map String (Map Int (Doc Name))
#if MIN_VERSION_haddock(2,10,0)
interfaceArgMap iface =
  M.fromList (map (first getOccString) (M.toList (instArgMap iface)))
#else
interfaceArgMap iface = M.fromList (map (second (const M.empty) . first getOccString)
                                        (M.toList (instDocMap iface)))
#endif
-- | Search for a module's package, returning suggestions if not
-- found.
getPackagesByModule :: DynFlags -> ModuleName -> IO (Either [Module] [PackageConfig])
getPackagesByModule d m =
  return (fmap (map fst) (lookupModuleWithSuggestions d m))

-- | Get the Haddock interfaces of the given package.
getHaddockInterfacesByPackage :: PackageConfig -> IO [Either String InterfaceFile]
getHaddockInterfacesByPackage =
  mapM (readInterfaceFile freshNameCache) . haddockInterfaces

-- | Print the documentation of the arguments.
lookupArgs :: InstalledInterface -> String -> Ghc (Maybe [(Int, Doc String)])
lookupArgs interface name = do
  case M.lookup name (interfaceArgMap interface) of
    Nothing -> return Nothing
    Just argMap ->
      return (Just (map (second (fmap getOccString)) (M.toList argMap)))
