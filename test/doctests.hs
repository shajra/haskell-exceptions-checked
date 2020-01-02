module Main where


import qualified Build_doctests as B
import Control.Monad (join)
import Data.Foldable (traverse_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)


main :: IO ()
main = do
    -- 1
    traverse_ putStrLn args
    unsetEnv "GHC_ENVIRONMENT"
    doctest args
    
  where
    args = join 
        [ B.flags
        , B.pkgs
        , B.module_sources
        ]
