{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make an index from identifiers to modules.

-- module Haskell.Docs.Index where

import           Haskell.Docs.Cabal
import           Haskell.Docs.Ghc
import           Haskell.Docs.Haddock
import           Haskell.Docs.Types

import           Control.Arrow
import           Control.Exception
import           Control.Exception (try,IOException)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Either
import           Data.Function
import qualified Data.HashMap.Lazy as LM
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Map (Map)
import           Data.Monoid
import qualified Data.Serialize as Serial
import           Data.Text (Text,pack,unpack)
import qualified Data.Text.IO as T
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           GhcMonad (liftIO)
import           GhcMonad (liftIO,Ghc)
import           Name
import           PackageConfig
import           Packages
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

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
                  M.insertWith (\x y -> x <> " " <> y)
                               (pack name)
                               (pack pkg <> ":" <> pack mod) m)
               M.empty
               flatfile)

saveIndex :: Index -> IO ()
saveIndex i =
  do d <- getTemporaryDirectory
     L.writeFile (d </> indexFilename) mempty
     h <- openFile (d </> indexFilename) AppendMode
     forM_ (M.toList i)
           (\(ident,modules) -> T.hPutStrLn h (ident <> " " <> modules))
     hClose h

-- | Filename to read/write index to.
indexFilename :: FilePath
indexFilename = "haskell-docs.index"

-- | Lookup an entry in the index by identifier.
lookupInIndex
  :: L8.ByteString -- ^ The UTF-8 encoded identifier.
  -> IO (Maybe L8.ByteString)
lookupInIndex ident =
  do d <- getTemporaryDirectory
     h <- openFile (d </> indexFilename) ReadMode
     catch
       (fix (\loop ->
               do line <- L8.hGetLine h
                  if L8.takeWhile (/= ' ') line == ident
                     then do hClose h
                             return (Just (L8.drop 1 (L8.dropWhile (/= ' ') line)))
                     else loop))
       (\(e :: IOException) -> return Nothing)

main :: IO ()
main =
  do (arg:_) <- getArgs
     result <- lookupInIndex (L8.pack arg)
     print result
