{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make an index from identifiers to modules.

-- module Haskell.Docs.Index where

import           Haskell.Docs.Cabal
import           Haskell.Docs.Ghc
import           Haskell.Docs.Haddock
import           Haskell.Docs.Types

import qualified Data.Serialize as Serial
import           Control.Arrow
import           Control.Exception
import           Control.Exception (try,IOException)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Either
import           Data.Function
import qualified Data.HashMap.Lazy as LM
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Map (Map)
import           Data.Monoid
import           Data.Text (Text,pack,unpack)
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GhcMonad (liftIO)
import           GhcMonad (liftIO,Ghc)
import           Name
import           PackageConfig
import           Packages
import           System.Directory
import           System.FilePath

-- | An identifier index.
type Index = HashMap Text Text

-- | Generate a flat file of all package, module, name combinations.
generateFlatFile :: IO [(String, String, String)]
generateFlatFile =
  do packages <- getAllPackages
     fmap (concat . map explode . concat)
          (forM packages
                (\package ->
                   do files <- getHaddockInterfacesByPackage package
                      return
                        (concat
                           (map (map (\iface ->
                                        (sourcePackageId package,instMod iface,instExports iface)) .
                                 ifInstalledIfaces)
                                (rights files)))))
  where explode (pkg,mod,names) =
          map (showPackageName pkg
              ,moduleNameString (moduleName mod)
              ,)
              (map getOccString names)

-- | Print the complete flat file.
printFlatFile :: IO ()
printFlatFile =
  do i <- generateFlatFile
     forM_ i
           (\(pkg,mod,name) ->
              putStrLn
                (pkg ++ " " ++
                 mod ++ " " ++
                 name))

-- | Generate an identifier index.
generateIndex :: IO Index
generateIndex =
  do flatfile <- generateFlatFile
     evaluate
       (foldl' (\m (pkg,mod,name) ->
                  M.insertWith (<>)
                               (pack name)
                               (pack pkg <> ":" <> pack mod <> ",") m)
               M.empty
               flatfile)

-- | Save the index to file.
saveIndex :: Index -> IO ()
saveIndex i =
  do d <- getTemporaryDirectory
     L.writeFile (d </> indexFilename)
                 (encode i)

-- | Read the index from file.
readIndex :: IO (Maybe Index)
readIndex =
  do d <- getTemporaryDirectory
     exists <- doesFileExist (d </> indexFilename)
     if exists
        then do bytes <- L.readFile (d </> indexFilename)
                case decode bytes of
                  Nothing -> return Nothing
                  Just i -> return i
        else return Nothing

-- | Try to read an existing index or generate a new one, save and
-- read from that.
readOrGenerateIndex :: IO Index
readOrGenerateIndex =
  do existing <- readIndex
     case existing of
       Nothing ->
         do new <- generateIndex
            saveIndex new
            return new
       Just i -> return i

-- | Filename to read/write index to.
indexFilename :: FilePath
indexFilename = "haskell-docs-index.json"

main :: IO ()
main =
  do i <- readOrGenerateIndex
     print (M.size i)
