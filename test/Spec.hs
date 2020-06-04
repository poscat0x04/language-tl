module Main where

import Data.Aeson
import Language.TL.I64
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck bijection

bijection :: I64 -> Property
bijection i = decode (encode i) === Just i
