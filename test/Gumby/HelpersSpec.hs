{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}

module Gumby.HelpersSpec where


-- gumby
import           Gumby.Helpers

-- hspec
import           Test.Hspec
import           Test.Hspec.Expectations


spec :: Spec
spec = do
    it "lensField_to_underscores" $ do
        lensField_to_underscores "_"    "_hello"        `shouldBe` "hello"
        lensField_to_underscores "_"    "_helloHere"    `shouldBe` "hello_here"
        lensField_to_underscores "_foo" "_fooHelloHere" `shouldBe` "hello_here"

    it "camelCase_to_underscores" $ do
        camelCase_to_underscores "hello"     `shouldBe` "hello"
        camelCase_to_underscores "helloHere" `shouldBe` "hello_here"
        camelCase_to_underscores "HelloHere" `shouldBe` "hello_here"