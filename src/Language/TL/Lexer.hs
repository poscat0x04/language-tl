module Language.TL.Lexer where

import Control.Applicative (liftA2)
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, cons)
import Data.Void
import Language.TL.Types
import Text.Megaparsec hiding (many, optional, sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | remove whitespaces
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

many :: Parser a -> Parser [a]
many = many' . lexeme

some :: Parser a -> Parser (NonEmpty a)
some p = (:|) <$> p <*> many p

many' :: Parser a -> Parser [a]
many' v = many_v
  where
    many_v = try some_v <|> pure []
    some_v = liftA2 (:) v many_v

optional :: Parser a -> Parser (Maybe a)
optional = optional' . lexeme

optional' :: Parser a -> Parser (Maybe a)
optional' v = Just <$> try v <|> pure Nothing

lexeme :: Parser a -> Parser a
lexeme = (sc >>)

string_ :: Text -> Parser Text
string_ = lexeme . string

char_ :: Char -> Parser Char
char_ = lexeme . char

between' :: Parser open -> Parser close -> Parser a -> Parser a
between' o c p = between o (lexeme c) (lexeme p)

tilLineEnd :: Parser Text
tilLineEnd =
  takeWhileP (Just "Line content") (/= '\n')

blockComment :: Parser Text
blockComment = do
  string "/*"
  blockCommentBody

blockCommentBody :: Parser Text
blockCommentBody = do
  c <- takeWhileP (Just "Block comment content") (/= '*')
  c' <- (string "*/" *> pure "") <|> do
    ch <- char '*'
    bc <- blockCommentBody
    pure (cons ch bc)
  pure (c <> c')

lineComment :: Parser Text
lineComment = do
  string "//"
  tilLineEnd

comment :: Parser Comment
comment = bc <|> lc
  where
    bc = BlockComment <$> blockComment
    lc = LineComment <$> lineComment

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_'

lcIdent :: Parser Text
lcIdent = do
  h <- lowerChar
  t <- takeWhileP (Just "ident-char") identChar
  pure $ cons h t

lcIdent' :: Parser Ident
lcIdent' = do
  ident <- lcIdent
  pure Unqualified {casing = L, ..}

ucIdent :: Parser Text
ucIdent = do
  h <- upperChar
  t <- takeWhileP (Just "ident-char") identChar
  pure $ cons h t

ucIdent' :: Parser Ident
ucIdent' = do
  ident <- ucIdent
  pure Unqualified {casing = U, ..}

nsIdent :: Parser Text
nsIdent = lcIdent

lcNsIdent :: Parser Ident
lcNsIdent = try unqualified <|> qualified
  where
    casing = L
    qualified = do
      ns <- nsIdent
      char '.'
      ident <- lcIdent
      pure Qualified {..}
    unqualified = do
      ident <- lcIdent
      pure Unqualified {..}

ucNsIdent :: Parser Ident
ucNsIdent = try unqualified <|> qualified
  where
    casing = U
    qualified = do
      ns <- nsIdent
      char '.'
      ident <- ucIdent
      pure Qualified {..}
    unqualified = do
      ident <- ucIdent
      pure Unqualified {..}

lcFullIdent :: Parser FullIdent
lcFullIdent = do
  ident <- lcNsIdent
  name <- optional $ do
    char '#'
    L.hexadecimal
  pure $ FullName ident name

finalKw :: Parser ()
finalKw = do
  string "Final"
  pure ()

newKw :: Parser ()
newKw = do
  string "New"
  pure ()

emptyKw :: Parser ()
emptyKw = do
  string "Empty"
  pure ()

nat :: Parser Int
nat = L.decimal

sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 p sep = liftA2 (:|) p (many (lexeme sep *> lexeme p))
