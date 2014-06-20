-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).

module Haskell.Docs
  (printDocumentationInitialized
  ,withInitializedPackages
  ,printDocumentation
  ,makeModuleName
  ,findIdentifier)
  where

import           Haskell.Docs.Formatting
import           Haskell.Docs.Ghc
import           Haskell.Docs.Haddock

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Loops
import           Data.Either
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GhcMonad (liftIO)
import           Name
import           PackageConfig

-- | Print documentation with an initialized package set.
printDocumentationInitialized :: String -> ModuleName -> Maybe String -> [String] -> IO Bool
printDocumentationInitialized x y z ghcopts =
  withInitializedPackages ghcopts $ \d ->
    printDocumentation d x y z Nothing

-- | Print the documentation of a name in the given module.
printDocumentation :: DynFlags -> String -> ModuleName -> Maybe String -> Maybe PackageConfig -> Ghc Bool
printDocumentation d name mname mpname previous = do
  result <- liftIO (getPackagesByModule d mname)
  case result of
    Left _suggestions -> error "Couldn't find that module. Suggestions are forthcoming."
    Right [package]   -> printWithPackage d False name mname package
    Right packages    ->
      case mpname of
        Nothing -> do
          liftIO (putStrLn $ "Ambiguous module, belongs to more than one package: " ++
                             unwords (map (showPackageName . sourcePackageId) packages) ++
                             "\nContinuing anyway... ")
          anyM (printWithPackage d True name mname) (filter (not . isPrevious) packages)
        Just pname -> do
          case find ((== pname) . showPackageName . sourcePackageId) packages of
            Nothing -> error "Unable to find that module/package combination."
            Just package -> printWithPackage d False name mname package

  where isPrevious m = Just (sourcePackageId m) == fmap sourcePackageId previous

-- | Print the documentation with the given package.
printWithPackage :: DynFlags -> Bool -> String -> ModuleName -> PackageConfig -> Ghc Bool
printWithPackage d printPackage name mname package = do
  interfaceFiles <- liftIO (getHaddockInterfacesByPackage package)
  case (lefts interfaceFiles,rights interfaceFiles) of
    ([],[])        -> error "Found no interface files."
    (errs@(_:_),_) -> error $ "Couldn't parse interface file(s): " ++ unlines errs
    (_,files)      ->
      flip anyM files $ \interfaceFile ->
        case filter ((==mname) . moduleName . instMod) (ifInstalledIfaces interfaceFile) of
          [] -> error "Couldn't find an interface for that module in the package description."
          interfaces -> anyM (printWithInterface d printPackage package name mname) interfaces

-- | Print the documentation from the given interface.
printWithInterface :: DynFlags -> Bool -> PackageConfig -> String -> ModuleName -> InstalledInterface
                   -> Ghc Bool
printWithInterface df printPackage package name mname interface = do
  case find ((==name).getOccString) (instExports interface) of
    Nothing -> bail
    Just{} ->
      case M.lookup name docMap of
        Nothing -> do
          case lookup name (map (getOccString &&& id) (instExports interface)) of
            Just subname
              | moduleName (nameModule subname) /= moduleName (instMod interface) ->
                descendSearch df name subname package
            _ -> bail
        Just d ->
          do liftIO (when printPackage $
                       putStrLn $ "Package: " ++ showPackageName (sourcePackageId package))
             mi <-  findIdentifier mname name
             case mi of
               Nothing -> return ()
               Just i -> liftIO (do putStr (showppr d i ++ " :: ")
                                    putStrLn (showppr d (idType i)))
             liftIO (putStrLn (formatDoc d))
             args <- lookupArgs interface name
             liftIO (putStr (unlines (map (\(i,x) -> formatArg i x)
                                          (fromMaybe [] args))))
             return True

  where docMap = interfaceNameMap interface
        bail = do
          liftIO (putStrLn $ "Couldn't find name ``" ++ name ++ "'' in Haddock interface: " ++
                             moduleNameString (moduleName (instMod interface)))
          return False

-- | The module symbol doesn't actually exist in the module we
-- intended, so we descend into the module that it does exist in and
-- restart our search process.
descendSearch :: DynFlags -> String -> Name -> PackageConfig -> Ghc Bool
descendSearch d name qname package = do
  printDocumentation d name (moduleName (nameModule qname)) Nothing (Just package)

-- | Make a module name.
makeModuleName :: String -> ModuleName
makeModuleName = mkModuleName
