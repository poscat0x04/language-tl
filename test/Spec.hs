module Main where

import qualified Data.Text.IO as T
import Language.TL.Parser
import Text.Megaparsec

main :: IO ()
main = do
  f <- T.readFile "test/data/td_api.tl"
  let r = runParser program "td_api" f
  case r of
    Left _ -> error "error: parse failed"
    Right a -> print a
  f2 <- T.readFile "test/data/telegram_api.tl"
  let r2 = runParser program "telegram_api" f2
  case r2 of
    Left _ -> error "error: parse failed"
    Right a -> print a
  f3 <- T.readFile "test/data/mtproto_api.tl"
  let r3 = runParser program "mtproto_api" f3
  case r3 of
    Left _ -> error "error: parse failed"
    Right a -> print a
