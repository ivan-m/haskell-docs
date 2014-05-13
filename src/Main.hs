{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
#if __GLASGOW_HASKELL__ < 706
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
import           qualified SrcLoc
import           Control.Exception (Exception, Handler(..), ErrorCall(..))
import           qualified Control.Exception as E
import           Data.Version (showVersion)
import           System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import           qualified System.Console.GetOpt as O
import           Data.Typeable (Typeable)
import           System.Exit (exitFailure)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           Paths_haskell_docs

-- Command line argument parsing code (from this point down to main, and including the body of main)
-- were adapted from ghc-mod's code: https://github.com/kazu-yamamoto/ghc-mod/blob/master/src/GHCMod.hs
data Options = Options { ghcOpts :: [String] }

defaultOptions :: Options
defaultOptions = Options { ghcOpts = [] }

progVersion :: String
progVersion = "haskell-docs version " ++ showVersion version ++ "\n"

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t haskell-docs getdoc" ++ ghcOptHelp ++ "<modulename> <name> [<package name>]\n"
        ++ "\t haskell-docs version\n"
        ++ "\t haskell-docs help\n"
        ++ "\n"

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "g" ["ghcOpt"]
            (ReqArg (\g opts -> opts { ghcOpts = g : ghcOpts opts }) "ghcOpt")
            "GHC options"
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case O.getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> E.throw (CmdArg errs)

data HaskellDocsError = SafeList
                 | WrongNrArguments String
                 | NoSuchCommand String
                 | CmdArg [String] deriving (Show, Typeable)

instance Exception HaskellDocsError

-- | Main entry point.
main :: IO ()
main = flip E.catches handlers $ do
    args <- getArgs
    let (opt, cmdArg) = parseArgs argspec args
    let cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg2 = cmdArg !. 2
        cmdArg3 = cmdArg !. 3
        remainingArgs = tail cmdArg
    case cmdArg0 of
      "getdoc"     -> case (length remainingArgs) of
                         2 -> withInitializedPackages (ghcOpts opt) $ \d ->
                                        void $ printDocumentation d cmdArg2 (mkModuleName cmdArg1) Nothing Nothing
                         3 -> withInitializedPackages (ghcOpts opt) $ \d ->
                                        void $ printDocumentation d cmdArg2 (mkModuleName cmdArg1) (Just cmdArg3) Nothing
                         _ -> E.throw (WrongNrArguments cmdArg0)
      "help"    -> putStrLn $ O.usageInfo usage argspec
      "version" -> putStrLn progVersion
      cmd       -> E.throw (NoSuchCommand cmd)
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: HaskellDocsError -> IO ()
    handler2 SafeList = printUsage
    handler2 (WrongNrArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Wrong number of arguments"
        printUsage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        printUsage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
        printUsage
    printUsage = hPutStrLn stderr $ '\n' : O.usageInfo usage argspec
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx

-- | Print documentation with an initialized package set.
printDocumentationInitialized :: [String] -> String -> ModuleName -> Maybe String -> IO Bool
printDocumentationInitialized opts x y z =
  withInitializedPackages opts $ \d ->
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
#if __GLASGOW_HASKELL__ < 708
  go (DocPic pic) = pic
#else
  go (DocPic pic) = show pic
#endif
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
withInitializedPackages :: [String] -> (DynFlags -> IO a) -> IO a
withInitializedPackages opts cont = do
#if __GLASGOW_HASKELL__ < 706
  dflags <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
#else
  dflags <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
#endif
    dflags <- getSessionDynFlags

    (dflags', _, _) <- parseDynamicFlags dflags (map SrcLoc.noLoc opts)

    setSessionDynFlags dflags'

    return dflags'
  (dflags,packageids) <- initPackages dflags
  cont dflags

--------------------------------------------------------------------------------
-- Utilities and missing instances

instance Show ModuleName where show = show . moduleNameString
instance Show OccName where show = show . occNameString
#if __GLASGOW_HASKELL__ < 708
deriving instance Show (Doc String)
#else
instance Show (Doc String) where show = formatDoc
#endif
