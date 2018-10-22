{-# OPTIONS_GHC -fdefer-type-errors #-}


module Specs.Checked where


import           Test.Hspec              (describe, it)
import qualified Test.Hspec              as Hspec
import           Test.ShouldNotTypecheck (shouldNotTypecheck)


spec :: Hspec.Spec
spec =
    describe "Checked" $ do
        it "can compile" $ do
            shouldNotTypecheck ("testing out framework" :: Int)
