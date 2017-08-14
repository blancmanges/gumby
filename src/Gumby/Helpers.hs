{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
module Gumby.Helpers where


-- base
import           Data.Word (Word8(..))
import           Data.List
import           Data.Char
import           Data.Function


lensField_to_underscores :: String -> String -> String
lensField_to_underscores pref x =
    case stripPrefix pref x of
        Just xs -> camelCase_to_underscores xs
        Nothing -> error $ "lensField_to_underscores: input doesn't have prefix '" ++ pref ++ "': " ++ x


camelCase_to_underscores :: String -> String
camelCase_to_underscores = clean . go . groupBy ((==) `on` isUpper)
  where
    clean :: String -> String
    clean ('_':xs) = xs
    clean xs = xs

    go :: [String] -> String
    go (chs:xs)
        | all isUpper chs = '_' : fmap toLower chs ++ go xs
        | otherwise  = chs ++ go xs
    go [] = []
