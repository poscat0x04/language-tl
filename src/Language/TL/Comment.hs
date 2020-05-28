module Language.TL.Comment where

import Data.Text (Text)
import Language.TL.Lexer hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char

type Attr = (Text, Text)

attr :: Parser Attr
attr = do
  char '@'
  tag <- takeWhileP (Just "Attr tag") (/= ' ')
  space
  content <- takeWhileP (Just "Attr content") (/= '@')
  pure $ (tag, content)

attrs :: Parser [Attr]
attrs = many attr
