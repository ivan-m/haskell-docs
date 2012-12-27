{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).
--
-- To build:
--
--     $ ghc --make HaskellDocs.hs -package ghc -o haskell-docs
--
-- Example usage:
--
--     $ ./haskell-docs System.IO getContents base
--     The “getContents” operation returns all user input as a single string,
--      which is read lazily as it is needed
--      (same as “hGetContents” “stdin”).
--
-- Using with GHCi
--
-- Move haskell-docs to ~/.cabal/bin. Then run the following in GHCi:
--
--     λ> :def doc \input -> return (":!haskell-docs " ++ input)
--
--     λ> :doc System.IO getContents base
--     The “getContents” operation returns all user input as a single string,
--      which is read lazily as it is needed
--      (same as “hGetContents” “stdin”).
--
-- Add the above :def to your ~/.ghci to have it on start-up.

module Main where

import           Control.Arrow
import           Control.Monad
import Control.Monad.Loops
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Documentation.Haddock
#if __GLASGOW_HASKELL__ <= 706
import           DynFlags (defaultLogAction)
#else
import           DynFlags (defaultFlushOut, defaultFatalMessager)
#endif
import           GHC hiding (flags, verbosity)
import           GHC.Paths (libdir)
import           Module
import           Name
import           PackageConfig
import           Packages
import           System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mname,name,pname] -> withInitializedPackages $ \d -> void $
                            printDocumentation d name (mkModuleName mname) (Just pname) Nothing
    [mname,name] -> withInitializedPackages $ \d -> void $
                      printDocumentation d name (mkModuleName mname) Nothing Nothing
    _ -> error "arguments: <modulename> <name> [<package name>]"

-- | Print documentation with an initialized package set.
printDocumentationInitialized :: String -> ModuleName -> Maybe String -> IO Bool
printDocumentationInitialized x y z =
  withInitializedPackages $ \d ->
    printDocumentation d x y z Nothing

-- | Print the documentation of a name in the given module.
printDocumentation :: DynFlags -> String -> ModuleName -> Maybe String -> Maybe PackageConfig -> IO Bool
printDocumentation d name mname mpname previous = do
  result <- getPackagesByModule d mname
  case result of
    Left _suggestions -> error "Couldn't find that module. Suggestions are forthcoming."
    Right [package]   -> printWithPackage d False name mname package
    Right packages    ->
      case mpname of
        Nothing -> do
          putStrLn $ "Ambiguous module, belongs to more than one package: " ++
                     unwords (map (showPackageName . sourcePackageId) packages) ++
                     "\nContinuing anyway... "
          anyM (printWithPackage d True name mname) (filter (not . isPrevious) packages)
        Just pname -> do
          case find ((== pname) . showPackageName . sourcePackageId) packages of
            Nothing -> error "Unable to find that module/package combination."
            Just package -> printWithPackage d False name mname package

  where isPrevious m = Just (sourcePackageId m) == fmap sourcePackageId previous

-- | Show the package name e.g. base.
showPackageName :: PackageIdentifier -> String
showPackageName = packageIdString . mkPackageId

-- | Print the documentation with the given package.
printWithPackage :: DynFlags -> Bool -> String -> ModuleName -> PackageConfig -> IO Bool
printWithPackage d printPackage name mname package = do
  interfaceFiles <- getHaddockInterfacesByPackage package
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
                   -> IO Bool
printWithInterface d printPackage package name mname interface = do
  case M.lookup name docMap of
    Nothing -> do
      case lookup name (map (getOccString &&& id) (instExports interface)) of
        Just subname
          | moduleName (nameModule subname) /= moduleName (instMod interface) ->
            descendSearch d name subname package
        _ -> do
          putStrLn $ "Couldn't find name ``" ++ name ++ "'' in Haddock interface: " ++
                     moduleNameString (moduleName (instMod interface))
          return False
    Just doc -> do when printPackage $
                     putStrLn $ "Package: " ++ showPackageName (sourcePackageId package)
                   putStrLn (formatDoc doc)
                   printArgs interface name
                   return True

  where docMap = interfaceNameMap interface

        printName x = moduleNameString (moduleName (nameModule x)) ++ "." ++ getOccString x

-- | Print the documentation of the arguments.
printArgs :: InstalledInterface -> String -> IO ()
printArgs interface name = do
  case M.lookup name (interfaceArgMap interface) of
    Nothing -> return ()
    Just argMap ->
      putStr $ unlines
            $ map (\(i,x) -> formatArg i x)
                  (map (second (fmap getOccString)) (M.toList argMap))

  where formatArg i x = prefix ++
                        indentAfter (length prefix) (formatDoc x)
          where prefix = show i ++ ": "

-- | Indent after the first line.
indentAfter :: Int -> String -> String
indentAfter i xs = intercalate "\n" (take 1 l ++ map (replicate (i-1) ' ' ++) (drop 1 l))
  where l = lines xs

-- | The module symbol doesn't actually exist in the module we
-- intended, so we descend into the module that it does exist in and
-- restart our search process.
descendSearch :: DynFlags -> String -> Name -> PackageConfig -> IO Bool
descendSearch d name qname package = do
  printDocumentation d name (moduleName (nameModule qname)) Nothing (Just package)

--------------------------------------------------------------------------------
-- Printing documentation

-- | Format some documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . go where
  go DocEmpty = ""
  go (DocAppend a b) = go a ++ go b
  go (DocString str) = normalize str
  go (DocParagraph p) = go p ++ "\n"
  go (DocIdentifier i) = i
  go (DocIdentifierUnchecked (mname,occname)) =
    moduleNameString mname ++ "." ++ occNameString occname
  go (DocModule m) = m
  go (DocEmphasis e) = "*" ++ go e ++ "*"
  go (DocMonospaced e) = "`" ++ go e ++ "`"
  go (DocUnorderedList i) = unlines (map (("* " ++) . go) i)
  go (DocOrderedList i) = unlines (zipWith (\i x -> show i ++ ". " ++ go x) [1..] i)
  go (DocDefList xs) = unlines (map (\(i,x) -> go i ++ ". " ++ go x) xs)
  go (DocCodeBlock block) = unlines (map ("    " ++) (lines (go block))) ++ "\n"
#if MIN_VERSION_haddock(2,13,1)
  go (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
#else
  go (DocURL url) = url
#endif
  go (DocPic pic) = pic
  go (DocAName name) = name
  go (DocExamples exs) = unlines (map formatExample exs)

  normalize = go where
    go (' ':' ':cs) = go (' ':cs)
    go (c:cs)       = c : go cs
    go []           = []

-- | Trim either side of a string.
trim :: [Char] -> [Char]
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Format an example to plain text.
formatExample :: Example -> String
formatExample (Example expression result) =
  "    > " ++ expression ++
  unlines (map ("    " ++) result)

--------------------------------------------------------------------------------
-- Package querying functions

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
interfaceNameMap iface =
  M.fromList (map (second (fmap getOccString) . first getOccString)
             (M.toList (instDocMap iface)))

-- | Get a mapping from names to doc string of that name from a
-- Haddock interface.
interfaceArgMap :: InstalledInterface -> Map String (Map Int (Doc Name))
interfaceArgMap iface =
  M.fromList (map (first getOccString) (M.toList (instArgMap iface)))

-- | Search for a module's package, returning suggestions if not
-- found.
getPackagesByModule :: DynFlags -> ModuleName -> IO (Either [Module] [PackageConfig])
getPackagesByModule d m =
  return (fmap (map fst) (lookupModuleWithSuggestions d m))

-- | Get the Haddock interfaces of the given package.
getHaddockInterfacesByPackage :: PackageConfig -> IO [Either String InterfaceFile]
getHaddockInterfacesByPackage = mapM (readInterfaceFile freshNameCache) . haddockInterfaces

-- | Run an action with an initialized GHC package set.
withInitializedPackages :: (DynFlags -> IO a) -> IO a
withInitializedPackages cont = do
#if __GLASGOW_HASKELL__ <= 706
  dflags <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
#else
  dflags <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
#endif
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    return dflags
  (dflags,packageids) <- initPackages dflags
  cont dflags

--------------------------------------------------------------------------------
-- Utilities and missing instances

instance Show ModuleName where show = show . moduleNameString
instance Show OccName where show = show . occNameString
deriving instance Show (Doc String)
