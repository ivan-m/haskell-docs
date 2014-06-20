{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- | Haddock compatibilty layer and query functions.

module Haskell.Docs.Haddock where

import           Haskell.Docs.Ghc
import           Haskell.Docs.Types

import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GhcMonad (liftIO)
import           Name
import           PackageConfig
import           Packages

-- * Searching for ident docs

-- | Search a name in the given module.
search
  :: Maybe PackageConfig
  -> Maybe PackageName
  -> ModuleName
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
search mprevious mpname mname name = do
  result <- getPackagesByModule mname
  case result of
    Left{} ->
      return (Left NoFindModule)
    Right [package]   ->
      searchWithPackage package mname name
    Right packages  ->
      case mpname of
        Nothing -> do
          fmap (Right . concat . rights)
               (mapM (\package -> searchWithPackage package mname name)
                     (filter (not . isPrevious) packages))
        Just pname -> do
          case find ((== pname) . PackageName . showPackageName . sourcePackageId) packages of
            Nothing ->
              return (Left NoModulePackageCombo)
            Just package ->
              searchWithPackage package mname name
  where isPrevious m =
          Just (sourcePackageId m) == fmap sourcePackageId mprevious

-- | Search for the given identifier in the given package.
searchWithPackage
  :: PackageConfig
  -> ModuleName
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
searchWithPackage package mname name = do
  interfaceFiles <- getHaddockInterfacesByPackage package
  case (lefts interfaceFiles,rights interfaceFiles) of
    ([],[])        -> return (Left NoInterfaceFiles)
    (errs@(_:_),_) -> return (Left (NoParseInterfaceFiles errs))
    (_,files)      ->
      fmap (Right . concat)
           (forM files
                 (\interfaceFile ->
                    fmap (concat . rights)
                         (mapM (searchWithInterface package mname name)
                               (filter ((==mname) . moduleName . instMod)
                                       (ifInstalledIfaces interfaceFile)))))

-- | Search for the given identifier in the interface.
searchWithInterface
  :: PackageConfig
  -> ModuleName
  -> Identifier
  -> InstalledInterface
  -> Ghc (Either DocsException [IdentDoc])
searchWithInterface package mname name interface =
  case find ((==name) . Identifier . getOccString) (instExports interface) of
    Nothing ->
      return (Left NoFindNameInExports)
    Just{} ->
      case M.lookup (unIdentifier name) (interfaceNameMap interface) of
        Nothing ->
          case lookup (unIdentifier name) (map (getOccString &&& id) (instExports interface)) of
            Just subname
              | moduleName (nameModule subname) /= moduleName (instMod interface) ->
                descendSearch package name subname
            _ ->
              return (Left NoFindNameInInterface)
        Just d ->
          do mi <- findIdentifier mname name
             margs <- lookupArgsDocs interface name
             return
               (Right
                  [IdentDoc (sourcePackageId package)
                            d
                            mi
                            margs])

-- * Get documentation of parts of things

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

-- | Find arguments documentation for the identifier.
lookupArgsDocs :: InstalledInterface -> Identifier -> Ghc (Maybe [(Int, Doc String)])
lookupArgsDocs interface name = do
  case M.lookup (unIdentifier name) (interfaceArgMap interface) of
    Nothing -> return Nothing
    Just argMap ->
      return (Just (map (second (fmap getOccString)) (M.toList argMap)))

-- * Querying for packages and interfaces

-- | Search for a module's package, returning suggestions if not
-- found.
getPackagesByModule :: ModuleName -> Ghc (Either [Module] [PackageConfig])
getPackagesByModule m =
  do df <- getSessionDynFlags
     return (fmap (map fst) (lookupModuleWithSuggestions df m))

-- | Get the Haddock interfaces of the given package.
getHaddockInterfacesByPackage :: PackageConfig -> Ghc [Either DocsException InterfaceFile]
getHaddockInterfacesByPackage =
  liftIO .
  mapM (fmap (either (Left . NoReadInterfaceFile) Right) . readInterfaceFile freshNameCache) .
  haddockInterfaces

-- * Internal functions

-- | The module symbol doesn't actually exist in the module we
-- intended, so we descend into the module that it does exist in and
-- restart our search process.
descendSearch :: PackageConfig -> Identifier -> Name -> Ghc (Either DocsException [IdentDoc])
descendSearch package name qname = do
  search (Just package) Nothing (moduleName (nameModule qname)) name
