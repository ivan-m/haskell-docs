{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- | Ghc compatibility layer.

module Haskell.Docs.Ghc where

import           Haskell.Docs.Types

import           Control.Exception (SomeException)
import           GHC hiding (verbosity)
import           GHC.Paths (libdir)
import           GhcMonad (liftIO)
import           Module
import           Name
import           Outputable
import           Packages
import qualified SrcLoc

#if __GLASGOW_HASKELL__ < 706
import           DynFlags (defaultLogAction)
#else
import           DynFlags (defaultFlushOut, defaultFatalMessager)
#endif

-- * GHC actions

-- | Run an action with an initialized GHC package set.
withInitializedPackages :: [String] -> Ghc a -> IO a
withInitializedPackages ghcopts m =
  run (do dflags <- getSessionDynFlags
          (dflags', _, _) <- parseDynamicFlags dflags (map SrcLoc.noLoc ghcopts)
          _ <- setSessionDynFlags (dflags' { hscTarget = HscInterpreted
                                           , ghcLink = LinkInMemory })
          (dflags'',_packageids) <- liftIO (initPackages dflags')
          _ <- setSessionDynFlags dflags''
          m)

-- | Get the type of the given identifier from the given module.
findIdentifier :: ModuleName -> Identifier -> Ghc (Maybe Id)
findIdentifier mname name =
  gcatch (do _ <- depanal [] False
             _ <- load LoadAllTargets
             setImportContext mname
             names <- getNamesInScope
             mty <- lookupName (head (filter ((==unIdentifier name).getOccString) names))
             case mty of
               Just (AnId i) -> return (Just i)
               _ -> return Nothing)
         (\(_ :: SomeException) -> return Nothing)

-- | Make a module name.
makeModuleName :: String -> ModuleName
makeModuleName = mkModuleName

-- * Internal functions

-- | Run the given GHC action.
#if __GLASGOW_HASKELL__ < 706
run :: Ghc a -> IO a
run = defaultErrorHandler defaultLogAction . runGhc (Just libdir)
#else
run :: Ghc a -> IO a
run = defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhc (Just libdir)
#endif

-- | Pretty print something to string.
showppr dflags = Haskell.Docs.Ghc.showSDocForUser dflags neverQualify . ppr

-- | Wraps 'Outputable.showSDocForUser'.
#if __GLASGOW_HASKELL__ == 702
showSDocForUser _ = Outputable.showSDocForUser
#endif
#if __GLASGOW_HASKELL__ == 704
showSDocForUser _ = Outputable.showSDocForUser
#endif
#if __GLASGOW_HASKELL__ == 706
showSDocForUser = Outputable.showSDocForUser
#endif
#if __GLASGOW_HASKELL__ == 708
showSDocForUser = Outputable.showSDocForUser
#endif

-- | Set the import context.
setImportContext :: ModuleName -> Ghc ()
#if __GLASGOW_HASKELL__ == 702
setImportContext mname = setContext [] [simpleImportDecl mname]
#else
setImportContext mname = setContext [IIDecl (simpleImportDecl mname)]
#endif

-- | Show the package name e.g. base.
showPackageName :: PackageIdentifier -> String
showPackageName = packageIdString . mkPackageId
