{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make an index from identifiers to modules.

module Haskell.Docs.Index where

import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text,pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Documentation.Haddock
import           GHC hiding (verbosity)
import           Haskell.Docs.Cabal
import           Haskell.Docs.Ghc
import           Haskell.Docs.Haddock
import           Name
import           PackageConfig
import           System.Directory
import           System.FilePath
import           System.IO

-- * Looking up identifiers

-- | Lookup an identifier. Automatically creates an index if none
-- exists.
lookupIdent :: Text
            -> IO (Maybe (HashMap Text [Text]))
lookupIdent ident =
  do d <- getTemporaryDirectory
     exists <- doesFileExist (d </> indexFilename)
     if exists
        then lookupInIndex ident
        else do generateIndex >>= saveIndex
                lookupInIndex ident

-- * Internally generating indexes

-- | An identifier index.
type Index = HashMap Text Text

-- | Generate an identifier index.
generateIndex :: IO Index
generateIndex =
  do flatfile <- generateFlatFile
     evaluate
       (foldl' (\m (pkg,modu,name) ->
                  M.insertWith (\x y -> x <> " " <> y)
                               (pack name)
                               (pack pkg <> ":" <> pack modu) m)
               M.empty
               flatfile)

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
  where explode (pkg,modu,names) =
          map (showPackageName pkg
              ,moduleNameString (moduleName modu)
              ,)
              (map getOccString names)

-- | Save the index to file.
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

-- * Internally looking up inside indexes

-- | Lookup an entry in the index by identifier.
lookupInIndex
  :: Text
  -> IO (Maybe (HashMap Text [Text]))
lookupInIndex (T.encodeUtf8 -> ident) =
  do d <- getTemporaryDirectory
     h <- openFile (d </> indexFilename) ReadMode
     catch
       (fix (\loop ->
               do line <- S.hGetLine h
                  if S.takeWhile (/= space) line == ident
                     then do hClose h
                             return (Just (extractModules (S.drop 1 (S.dropWhile (/= space) line))))
                     else loop))
       (\(_ :: IOException) -> return Nothing)
  where space = S.c2w ' '

-- | Extract the \"package:Module package:Module\" string into a map from package to modules.
extractModules :: ByteString -> HashMap Text [Text]
extractModules = foldl' ins mempty . mapMaybe unpair . chunks . T.decodeUtf8
  where chunks = T.split isSpace
        unpair t = case T.split (==':') t of
                     [package,modu] -> Just (package,modu)
                     _ -> Nothing
        ins m (pkg,modu) = M.insertWith (++) pkg [modu] m
