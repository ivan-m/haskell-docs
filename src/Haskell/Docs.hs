{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).

module Haskell.Docs
  (module Haskell.Docs
  ,Identifier(..)
  ,PackageName(..))
  where

import Haskell.Docs.Cabal
import Haskell.Docs.Formatting
import Haskell.Docs.Ghc
import Haskell.Docs.Haddock
import Haskell.Docs.Index
import Haskell.Docs.Types
import PackageConfig           hiding (PackageName)

import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Ord
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           GHC                 hiding (verbosity)
import           MonadUtils

-- -- | Print the documentation of a name in the given module.
searchAndPrintDoc
  :: [String]          -- ^ GHC Options
  -> Bool              -- ^ Print modules only.
  -> Bool              -- ^ S-expression format.
  -> Maybe PackageName -- ^ Package.
  -> Maybe ModuleName  -- ^ Module name.
  -> Identifier        -- ^ Identifier.
  -> Ghc ()
searchAndPrintDoc flags _ms ss _pname _mname ident =
  do result <- liftIO (lookupIdent flags
                                   (T.pack (unIdentifier ident)))
     case result of
       Nothing -> throw NoFindModule
       Just pkgModuleMap ->
         do pkgs <- getAllPackages flags
            docs <- fmap concat
                         (forM (M.toList pkgModuleMap)
                               (searchResult pkgs))
            if ss
               then printSexp docs
               else mapM_ (\(_,doc') ->
                             printIdentDoc False True True doc')
                          (zip [0 :: Int ..]
                               (nub docs))
  where searchResult pkgs (pkgName,modName) =
          case find (matchingPkg pkgName) pkgs of
            Nothing -> return []
            Just pkg -> searchPkg pkg modName
          where matchingPkg pkgNm = (== pkgNm) . T.pack . showPackageName .
                                    getIdentifier
        searchPkg pkg modName =
          do result <- searchWithPackage
                         pkg
                         (Just (head (fmap (makeModuleName . T.unpack) modName)))
                         ident
             case result of
               Left err -> throw err
               Right (sortBy (comparing identDocPackageName) -> docs) -> return docs

-- | Search only for identifiers and print out all modules associated.
searchAndPrintModules :: [String] -> Identifier -> IO ()
searchAndPrintModules flags ident =
  do result <- lookupIdent flags (T.pack (unIdentifier ident))
     case result of
       Nothing ->
         throw NoFindModule
       Just packages ->
         forM_ (nub (concat (map snd (M.toList packages))))
               T.putStrLn
