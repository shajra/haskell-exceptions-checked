module Main where


import qualified Build_doctests as B
import Control.Monad (join)
import Data.Foldable (toList, traverse_)
import System.Environment (getArgs, lookupEnv)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)


main :: IO ()
main = do
    unsetEnv "GHC_ENVIRONMENT"
    args <- toArgs <$> lookupEnv "DOCTEST_PKG_DB"
    traverse_ putStrLn args
    userArgs <- getArgs
    doctest $ args <> userArgs

  where
    toArgs :: Maybe String -> [String]
    toArgs mayPkgDb = join
        [ ("-package-db=" <>) <$> toList mayPkgDb
        , B.flags
        , B.pkgs
        , B.module_sources
        ]
