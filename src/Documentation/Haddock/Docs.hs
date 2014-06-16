{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).

module Documentation.Haddock.Docs where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Loops
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GHC.Paths (libdir)
import           GhcMonad (liftIO)
import           Module
import           Name
import           Outputable
import           PackageConfig
import           Packages
import qualified SrcLoc

#if __GLASGOW_HASKELL__ < 706
import           DynFlags (defaultLogAction)
#else
import           DynFlags (defaultFlushOut, defaultFatalMessager)
#endif

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

-- | Show the package name e.g. base.
showPackageName :: PackageIdentifier -> String
showPackageName = packageIdString . mkPackageId

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
    Just qname ->
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
             printType df mname qname name
             liftIO (putStrLn (formatDoc d))
             printArgs interface name
             return True

  where docMap = interfaceNameMap interface
        bail = do
          liftIO (putStrLn $ "Couldn't find name ``" ++ name ++ "'' in Haddock interface: " ++
                             moduleNameString (moduleName (instMod interface)))
          return False

printType d mname _qname name =
  do _graph <- depanal [] False
     _loaded <- load LoadAllTargets
#if __GLASGOW_HASKELL__ == 702
#else
     setContext [IIDecl (simpleImportDecl mname)]
#endif
     names <- getNamesInScope
     mty <- lookupName (head (filter ((==name).getOccString) names))
     case mty of
       Just (AnId i) -> liftIO (do putStr (showppr d i ++ " :: ")
                                   putStrLn (showppr d (idType i)))
       _ -> liftIO (putStrLn "Unable to find type for identifier.")

-- | Print the documentation of the arguments.
printArgs :: InstalledInterface -> String -> Ghc ()
printArgs interface name = do
  case M.lookup name (interfaceArgMap interface) of
    Nothing -> return ()
    Just argMap ->
      liftIO (putStr $ unlines
                     $ map (\(i,x) -> formatArg i x)
                           (map (second (fmap getOccString)) (M.toList argMap)))

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
descendSearch :: DynFlags -> String -> Name -> PackageConfig -> Ghc Bool
descendSearch d name qname package = do
  printDocumentation d name (moduleName (nameModule qname)) Nothing (Just package)

--------------------------------------------------------------------------------
-- Printing documentation

-- | Format some documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . doc where

-- | Render the doc.
doc :: Doc String -> String
doc DocEmpty = ""
doc (DocAppend a b) = doc a ++ doc b
doc (DocString str) = normalize str
doc (DocParagraph p) = doc p ++ "\n"
doc (DocModule m) = m
doc (DocEmphasis e) = "*" ++ doc e ++ "*"
doc (DocMonospaced e) = "`" ++ doc e ++ "`"
doc (DocUnorderedList i) = unlines (map (("* " ++) . doc) i)
doc (DocOrderedList i) = unlines (zipWith (\j x -> show j ++ ". " ++ doc x) [1 :: Int ..] i)
doc (DocDefList xs) = unlines (map (\(i,x) -> doc i ++ ". " ++ doc x) xs)
doc (DocCodeBlock block) = unlines (map ("    " ++) (lines (doc block))) ++ "\n"
doc (DocAName name) = name
doc (DocExamples exs) = unlines (map formatExample exs)

#if MIN_VERSION_haddock(2,10,0)
-- The header type is unexported, so this constructor is useless.
doc (DocIdentifier i) = i
doc (DocWarning d) = "Warning: " ++ doc d
#else
doc (DocPic pic) = pic
doc (DocIdentifier i) = intercalate "." i
#endif

#if MIN_VERSION_haddock(2,11,0)
doc (DocIdentifierUnchecked (mname,occname)) =
  moduleNameString mname ++ "." ++ occNameString occname
doc (DocPic pic) = show pic
#endif

#if MIN_VERSION_haddock(2,13,0)
doc (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
doc (DocProperty p) = "Property: " ++ p
#else
doc (DocURL url) = url
#endif

#if MIN_VERSION_haddock(2,14,0)
doc (DocBold d) = "**" ++ doc d ++ "**"
doc (DocHeader _) = ""
#endif

normalize :: [Char] -> [Char]
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

-- | Run an action with an initialized GHC package set.
withInitializedPackages :: [String] -> (DynFlags -> Ghc a) -> IO a
withInitializedPackages ghcopts cont =
  run (do dflags <- getSessionDynFlags
          (dflags', _, _) <- parseDynamicFlags dflags (map SrcLoc.noLoc ghcopts)
          _ <- setSessionDynFlags (dflags' { hscTarget = HscInterpreted
                                            , ghcLink = LinkInMemory })
          (dflags'',_packageids) <- liftIO (initPackages dflags')
          cont dflags'')

#if __GLASGOW_HASKELL__ < 706
run :: Ghc a -> IO a
run = defaultErrorHandler defaultLogAction . runGhc (Just libdir)
#else
run :: Ghc a -> IO a
run = defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhc (Just libdir)
#endif

showppr :: Outputable a => DynFlags -> a -> String
showppr dflags = Documentation.Haddock.Docs.showSDocForUser dflags neverQualify . ppr

sdoc :: DynFlags -> SDoc -> String
sdoc dflags = Documentation.Haddock.Docs.showSDocForUser dflags neverQualify

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
