{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | Lookup the documentation of a name in a module (and in a specific
-- package in the case of ambiguity).

module Haskell.Docs
  (module Haskell.Docs
  ,Identifier(..)
  ,PackageName(..)
  ,searchAndPrintModules
  ,searchAndPrintDoc)
  where

import           Haskell.Docs.Formatting
import           Haskell.Docs.Haddock
import           Haskell.Docs.Index
import           Haskell.Docs.Types

import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Ord
import           Data.Text (pack,unpack)
import qualified Data.Text.IO as T
import           GHC hiding (verbosity)
import           MonadUtils

-- -- | Print the documentation of a name in the given module.
searchAndPrintDoc
  :: Bool              -- ^ Print modules only.
  -> Bool              -- ^ S-expression format.
  -> Maybe PackageName -- ^ Package.
  -> Maybe ModuleName  -- ^ Module name.
  -> Identifier        -- ^ Identifier.
  -> Ghc ()
searchAndPrintDoc ms ss pname mname ident =
  do (result,printPkg,printModule) <- search
     case result of
       Left err ->
         throw err
       Right (sortBy (comparing identDocPackageName) -> docs) ->
          if ss
             then printSexp (nub docs)
             else mapM_ (\(i,doc') ->
                           do when (not ms && i > 0)
                                   (liftIO (putStrLn ""))
                              printIdentDoc ms printPkg printModule doc')
                        (zip [0::Int ..] (nub docs))
  where search =
          case (pname,mname) of
            (Just p,Just m) -> fmap (,False,False) (searchPackageModuleIdent Nothing p m ident)
            (Nothing,Just m) -> fmap (,True,False) (searchModuleIdent Nothing m ident)
            _ -> fmap (,True,True) (searchIdent Nothing ident)

-- | Search only for identifiers and print out all modules associated.
searchAndPrintModules :: [String] -> Identifier -> IO ()
searchAndPrintModules flags ident =
  do result <- lookupIdent flags (pack (unIdentifier ident))
     case result of
       Nothing ->
         throw NoFindModule
       Just packages ->
         forM_ (nub (concat (map snd (M.toList packages))))
               T.putStrLn
