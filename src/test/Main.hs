-- | Main test suite.

module Main where

import qualified Control.Exception as E
import           Control.Monad
import           Documentation.Haddock.Docs
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
        (withInitializedPackages [] (\dflags -> return ()))

-- | Test GHC docs.
docs :: IO ()
docs =
  do it "printDocumentation"
        (withInitializedPackages
           []
           (\d ->
              void (printDocumentation
                     d
                     "hSetBuffering"
                     (mkModuleName "System.IO")
                     Nothing
                     Nothing)))

-- | Test GHC types.
types :: IO ()
types =
  do it "getType"
        (withInitializedPackages
           []
           (\d ->
              do void (printDocumentation
                        d
                        "hSetBuffering"
                        (mkModuleName "System.IO")
                        Nothing
                        Nothing)
                 void (getType d
                               (mkModuleName "System.IO")
                               "hSetBuffering")))

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
