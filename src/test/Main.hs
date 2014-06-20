-- | Main test suite.

module Main where

import qualified Control.Exception as E
import           Control.Monad
import           Haskell.Docs
import           Haskell.Docs.Ghc
import           System.Exit
import           System.IO

-- | Main entry point.
main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     spec

-- | Test suite.
spec :: IO ()
spec =
  do describe "initialization" initialization
     describe "docs" docs
     describe "types" types

-- | Test GHC initialization.
initialization :: IO ()
initialization =
  do it "withInitializedPackages"
        (withInitializedPackages [] (return ()))

-- | Test GHC docs.
docs :: IO ()
docs =
  do it "printDocumentation"
        (withInitializedPackages
           []
           (void (searchAndPrintDoc
                    Nothing
                    (Just (makeModuleName "System.IO"))
                    (Identifier "hSetBuffering"))))
     it "justIdentifier"
        (withInitializedPackages
           []
           (void (searchAndPrintDoc
                    Nothing
                    Nothing
                    (Identifier "hSetBuffering"))))

-- | Test GHC types.
types :: IO ()
types =
  do it "getType"
        (withInitializedPackages
           []
           (do void (searchAndPrintDoc
                      Nothing
                      (Just (makeModuleName "System.IO"))
                      (Identifier "hSetBuffering"))
               void (findIdentifier (makeModuleName "System.IO")
                                    (Identifier "hSetBuffering"))))

-- | Describe a test spec.
describe :: String -> IO () -> IO ()
describe name m =
  do putStrLn ("Spec: " ++ name)
     m

-- | A test.
it :: String -> IO () -> IO ()
it name m =
  do putStr ("Testing: " ++ name)
     E.catch m
             (\E.SomeException{} ->
                do putStrLn ("\nFailed: " ++ name)
                   exitFailure)
     putStrLn "OK."
