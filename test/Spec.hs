module Main where

import qualified Data.Text.IO as T
import Language.TL.Parser
import Language.TL.Types
import Text.Megaparsec

main :: IO ()
main = do
  f <- T.readFile "test/data/td_api.tl"
  let Right res = runParser program "td_api.tl" f
  print $ head res
