module Language.TL.Lexer
  ( Parser,

    -- * Helper functions
    many,
    many',
    some,
    optional,
    lexeme,
    sepBy1,

    -- * Tokenizing
    string_,
    char_,
    between',
    comment,
    lcIdent',
    ucIdent',
    lcNsIdent,
    ucNsIdent,
    lcFullIdent,
    emptyKw,
    newKw,
    finalKw,
    nat,
  )
where

import Control.Applicative (liftA2)
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, cons)
import Data.Void
import Language.TL.Types
import Text.Megaparsec hiding (many, optional, sepBy1, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type
type Parser = Parsec Void Text

-- | remove whitespaces
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | 'many'' but removes whitespaces and comments before parsing each segment
many :: Parser a -> Parser [a]
many = many' . lexeme

-- | @some@ that backtracks and removes whitespaces etc.
some :: Parser a -> Parser (NonEmpty a)
some p = (:|) <$> p <*> many p

-- | @many@ that backtracks
many' :: Parser a -> Parser [a]
many' v = many_v
  where
    many_v = try some_v <|> pure []
    some_v = liftA2 (:) v many_v

-- | 'optional'' but removes whitespaces etc.
optional :: Parser a -> Parser (Maybe a)
optional = optional' . lexeme

-- | @optional@ that backtracks
optional' :: Parser a -> Parser (Maybe a)
optional' v = Just <$> try v <|> pure Nothing

-- | Removes whitespaces and comments before parsing
lexeme :: Parser a -> Parser a
lexeme = (sc >>)

-- | String lexeme
string_ :: Text -> Parser Text
string_ = lexeme . string

-- | Character lexeme
char_ :: Char -> Parser Char
char_ = lexeme . char

-- | 'between' but removes whitespaces etc.
between' :: Parser open -> Parser close -> Parser a -> Parser a
between' o c p = between o (lexeme c) (lexeme p)

-- | take til line end
tilLineEnd :: Parser Text
tilLineEnd =
  takeWhileP (Just "Line content") (/= '\n')

-- | parses block comment
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

-- | Parses line comment
lineComment :: Parser Text
lineComment = do
  string "//"
  tilLineEnd

-- | Parses a comment block
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

-- | Parses a lower case identfier
lcIdent' :: Parser Ident
lcIdent' = do
  ident <- lcIdent
  pure Unqualified {casing = L, ..}

ucIdent :: Parser Text
ucIdent = do
  h <- upperChar
  t <- takeWhileP (Just "ident-char") identChar
  pure $ cons h t

-- | Parses a upper case identifier
ucIdent' :: Parser Ident
ucIdent' = do
  ident <- ucIdent
  pure Unqualified {casing = U, ..}

nsIdent :: Parser Text
nsIdent = lcIdent

-- | Parses a lower case potentially qualified identifier
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

-- | Parses a upper case potentially qualified identifier
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

-- | Parses a lower case full identifier
lcFullIdent :: Parser FullIdent
lcFullIdent = do
  ident <- lcNsIdent
  name <- optional $ do
    char '#'
    L.hexadecimal
  pure $ FullName ident name

-- | Consumes keyword @Final@
finalKw :: Parser ()
finalKw = do
  string "Final"
  pure ()

-- | Consumes keyword @New@
newKw :: Parser ()
newKw = do
  string "New"
  pure ()

-- | Consumes keyword @Empty@
emptyKw :: Parser ()
emptyKw = do
  string "Empty"
  pure ()

-- | Parses a natural number
nat :: Parser Int
nat = L.decimal

-- | @sepBy1@ that backtracks and removes whitespaces etc.
sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 p sep = liftA2 (:|) p (many (lexeme sep *> lexeme p))
