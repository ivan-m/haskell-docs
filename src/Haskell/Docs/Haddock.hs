{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- | Haddock compatibilty layer and query functions.

module Haskell.Docs.Haddock where

import           Haskell.Docs.Cabal
import           Haskell.Docs.Ghc
import           Haskell.Docs.Types

import           Control.Arrow
import           Control.Exception (try,IOException)
import           Control.Monad
import           Data.Either
import           Data.Function
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
searchIdent
  :: Maybe PackageConfig
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
searchIdent mprevious name =
  do packages <- fmap (filterPrevious mprevious) (liftIO getAllPackages)
     searchInPackages packages
                      Nothing
                      name

-- | Search a name in the given module.
searchModuleIdent
  :: Maybe PackageConfig
  -> ModuleName
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
searchModuleIdent mprevious mname name =
  do result <- fmap (filterPrevious mprevious) (getPackagesByModule mname)
     case result of
       [] ->
         return (Left NoFindModule)
       [package] ->
         searchWithPackage package (Just mname) name
       packages ->
         searchInPackages packages
                          (Just mname)
                          name

-- | Search a name in the given module from the given package.
searchPackageModuleIdent
  :: Maybe PackageConfig
  -> PackageName
  -> ModuleName
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
searchPackageModuleIdent mprevious pname mname name =
  do result <- fmap (filterPrevious mprevious) (getPackagesByModule mname)
     case result of
       [] -> return (Left NoFindModule)
       packages ->
         case find ((== pname) . PackageName . showPackageName . sourcePackageId) packages of
           Nothing ->
             return (Left NoModulePackageCombo)
           Just package ->
             searchWithPackage package (Just mname) name

filterPrevious exclude =
  filter (maybe (const True)
                (on (/=) sourcePackageId)
                exclude)

-- | Search for the identifier in a module in any of the given packages.
searchInPackages
  :: [PackageConfig]
  -> Maybe ModuleName
  -> Identifier
  -> Ghc (Either a [IdentDoc])
searchInPackages packages mname name =
  fmap (Right . concat . rights)
       (mapM (\package -> searchWithPackage package mname name)
             packages)

-- | Search for the given identifier in the given package.
searchWithPackage
  :: PackageConfig
  -> Maybe ModuleName
  -> Identifier
  -> Ghc (Either DocsException [IdentDoc])
searchWithPackage package mname name =
  do interfaceFiles <- getHaddockInterfacesByPackage package
     case (lefts interfaceFiles,rights interfaceFiles) of
       ([],[])        ->
         return (Left NoInterfaceFiles)
       (errs@(_:_),_) ->
         return (Left (NoParseInterfaceFiles errs))
       (_,files)      ->
         fmap (Right . concat)
               (forM files
                     (\interfaceFile ->
                        fmap (concat . rights)
                             (mapM (searchWithInterface package name)
                                   (filter (maybe (const True)
                                                  (\n -> (==n) . moduleName . instMod)
                                                  mname)
                                           (ifInstalledIfaces interfaceFile)))))

-- | Search for the given identifier in the interface.
searchWithInterface
  :: PackageConfig
  -> Identifier
  -> InstalledInterface
  -> Ghc (Either DocsException [IdentDoc])
searchWithInterface package name interface =
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
          do mi <- findIdentifier (moduleName (instMod interface)) name
             margs <- lookupArgsDocs interface name
             return
               (Right
                  [IdentDoc (sourcePackageId package)
                            name
                            (moduleName (instMod interface))
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
-- found. Filters out the given value.
getPackagesByModule :: ModuleName -> Ghc [PackageConfig]
getPackagesByModule m =
  do df <- getSessionDynFlags
     return (either (const [])
                    (map fst)
                    (lookupModuleWithSuggestions df m))

-- | Get the Haddock interfaces of the given package.
getHaddockInterfacesByPackage :: PackageConfig -> Ghc [Either DocsException InterfaceFile]
getHaddockInterfacesByPackage =
  liftIO .
  mapM (fmap (either (Left . NoReadInterfaceFile) Right) . safelyReadFile freshNameCache) .
  haddockInterfaces
  where safelyReadFile cache p =
          do result <- try (readInterfaceFile cache p)
             case result of
               Left (_::IOException) -> return (Left "Couldn't read file.")
               Right r -> return r

-- * Internal functions

-- | The module symbol doesn't actually exist in the module we
-- intended, so we descend into the module that it does exist in and
-- restart our search process.
descendSearch :: PackageConfig -> Identifier -> Name -> Ghc (Either DocsException [IdentDoc])
descendSearch package name qname = do
  searchModuleIdent (Just package) (moduleName (nameModule qname)) name
