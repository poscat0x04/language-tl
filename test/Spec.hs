module Main where

import qualified Data.Text.IO as T
import Language.TL.Parser
import Text.Megaparsec

main :: IO ()
main = do
  f <- T.readFile "test/data/td_api.tl"
  parseTest program f
