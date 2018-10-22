module Main where


import qualified Test.Hspec    as Hspec

import qualified Specs.Checked as Checked


main :: IO ()
main = Hspec.hspec Checked.spec
