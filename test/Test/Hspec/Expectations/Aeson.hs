{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
module Test.Hspec.Expectations.Aeson (
    module ReExported
  , shouldDecodeTo
) where



-- hspec
import           Test.Hspec              as ReExported
import           Test.Hspec.Expectations as ReExported

-- lens
import           Control.Lens
import qualified Data.Text.Lazy.Lens as LsLazy
import qualified Data.Text.Strict.Lens as LsStrict

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- text
import qualified Data.Text as T

-- aeson
import qualified Data.Aeson as Ae



shouldDecodeTo
    :: (HasCallStack, Show a, Eq a, Ae.FromJSON a)
    => T.Text -> a -> Expectation
shouldDecodeTo txt expected = do
    let decoded = txt ^. re LsStrict.utf8 . to BL.fromStrict . to Ae.decode
    decoded `shouldBe` Just expected
