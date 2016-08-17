[![Hackage](https://img.shields.io/hackage/v/haskell-docs.svg?style=flat)](https://hackage.haskell.org/package/haskell-docs) [![Build Status](https://travis-ci.org/ivan-m/haskell-docs.svg?style=flat)](https://travis-ci.org/ivan-m/haskell-docs)

Given a module name and a name, it will find and display the
documentation of that name.

## Example usage

Call it with a module name and an identifier:

    $ haskell-docs Data.List.Split split
    split :: forall a. Splitter a -> [a] -> [[a]]
    Split a list according to the given splitting strategy. This is
     how to "run" a Splitter that has been built using the other
     combinators.

## Installation

You should ensure that you have

    documentation: True

in your `.cabal/config` so that the necessary .haddock files are
generated.

Haddock is very sensitive to the GHC version. This program tries not
to be. If you cannot install this package due to a version problem,
open a Github issue. If the versions match up but the build fails,
open a Github issue. Neither case should arise.

Supported GHC versions:

* ghc==7.8.* (requires haddock==2.14.*)
* ghc==7.6.* (requires haddock==2.13.*)
* ghc==7.4.* (requires haddock==2.11.*)
* ghc==7.2.* (requires haddock==2.9.*)

If you are using haddock==2.12 somehow, please open an issue about it.

## Using with GHCi

``` haskell
 > :def doc \input -> return (":!haskell-docs " ++ input)
 > :doc System.IO getContents base
```

Outputs:

> `getContents :: IO String`
> The `getContents` operation returns all user input as a single string,
> which is read lazily as it is needed
> (same as `hGetContents stdin`).

Add the above `:def` to your user `.ghci` to have it on start-up.

## Contribution and issues

Issues/ideas/contributions please make a Github issue:
http://github.com/chrisdone/haskell-docs/issues
