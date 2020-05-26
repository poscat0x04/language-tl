module Language.TL.Parser where

import Data.List.NonEmpty (toList)
import Data.Maybe (isJust)
import Language.TL.Lexer
import Language.TL.Types
import Text.Megaparsec hiding (many, optional, sepBy1, some)
import Text.Megaparsec.Char

orUnderscore :: Parser a -> Parser (Optional a)
orUnderscore p = try normal <|> underscore
  where
    normal = Optional <$> p
    underscore = do
      char '_'
      pure Omitted

---------

program :: Parser Program
program = do
  h <- constrDecls
  dl <- many $ fun <|> ty
  pure $ h : dl
  where
    fun = do
      string "---functions---"
      funDecls
    ty = do
      string "---types---"
      constrDecls

constrDecls :: Parser DeclBlock
constrDecls = TypeDeclBlk <$> many' annDecl

funDecls :: Parser DeclBlock
funDecls = FunDeclBlk <$> many' annDecl

annDecl :: Parser AnnDecl
annDecl = do
  comments <- many' $ space >> comment
  d <- lexeme decl
  pure $ AnnDecl comments d

decl :: Parser Decl
decl = try comb <|> try final <|> papp
  where
    comb = Combinator <$> combDecl
    papp = PartialApp <$> partialAppDecl
    final = FinalDecl <$> finalDecl

----------

typeExpr :: Parser Expr
typeExpr = expr

natExpr :: Parser Expr
natExpr = expr

expr :: Parser Expr
expr = Exprs <$> many subExpr

subExpr :: Parser SubExpr
subExpr =
  Sum <$> sepBy1 (term' <|> nat') (char '+')
  where
    term' = Right <$> try term
    nat' = Left <$> nat

term :: Parser Term
term =
  try expr'
    <|> try typeApp'
    <|> try typeIdent'
    <|> try varIdent'
    <|> try natConst'
    <|> try term'
  where
    expr' =
      Expr
        <$> between'
          (char '(')
          (char ')')
          expr
    typeIdent' = Type <$> typeIdent
    varIdent' = Var <$> varIdent
    natConst' = Nat <$> nat
    term' = do
      char '%'
      PTerm <$> lexeme term
    typeApp' = do
      tyIdent <- typeIdent
      char_ '<'
      exprs <- lexeme $ sepBy1 expr (char ',')
      char_ '>'
      pure $ TypeApp tyIdent exprs

typeIdent :: Parser TypeIdent
typeIdent = try boxed <|> try lc <|> nat
  where
    boxed = Boxed <$> boxedTypeIdent
    lc = LcIdent <$> lcIdent'
    nat = char '#' >> pure NatType

boxedTypeIdent :: Parser BoxedTypeIdent
boxedTypeIdent = ucNsIdent

varIdent :: Parser Ident
varIdent = try lcIdent' <|> ucIdent'

typeTerm :: Parser Term
typeTerm = term

natTerm :: Parser Term
natTerm = term

combDecl :: Parser CombinatorDecl
combDecl = try normal <|> builtin
  where
    normal = do
      combId <- fullCombId
      optArglist <- many optArgs
      argsList <- many args
      char_ '='
      resType <- lexeme resultType
      char_ ';'
      pure $ CombinatorDecl {..}
    builtin = do
      ident <- fullCombId
      char_ '?'
      char_ '='
      res <- lexeme boxedTypeIdent
      char_ ';'
      pure $ BuiltinDecl ident res

fullCombId :: Parser (Optional FullIdent)
fullCombId = orUnderscore lcFullIdent

combIdent :: Parser (Optional Ident)
combIdent = orUnderscore lcNsIdent

optArgs :: Parser OptArgs
optArgs = do
  char '{'
  ident <- some varIdent
  char_ ':'
  excl <- optional $ char '!'
  expr <- lexeme typeExpr
  char_ '}'
  pure $ OptArgs ident (isJust excl) expr

args :: Parser Args
args = try named <|> try multiple <|> try namedList <|> unamed
  where
    named = do
      ident <- varIdentOpt
      char_ ':'
      def <- optional conditionalDef
      excl <- optional $ char '!'
      tyTerm <- typeTerm
      pure $ Named ident def (isJust excl) tyTerm
    multiple = do
      ident <- optional $ varIdentOpt <* char_ ':'
      multi <- optional $ multiplicity <* char_ '*'
      char_ '['
      argl <- many args
      char_ ']'
      pure $ MultipleArgs ident multi argl
    namedList = do
      char '('
      idents <- some varIdentOpt
      char_ ':'
      excl <- optional $ char '!'
      tyTerm <- lexeme typeTerm
      char_ ')'
      pure $ NamedList idents (isJust excl) tyTerm
    unamed = do
      excl <- optional $ char '!'
      tyTerm <- lexeme typeTerm
      pure $ Unnamed (isJust excl) tyTerm

multiplicity :: Parser Term
multiplicity = natTerm

varIdentOpt :: Parser (Optional Ident)
varIdentOpt = orUnderscore varIdent

conditionalDef :: Parser ConditionalDef
conditionalDef = do
  ident <- varIdent
  n <- optional $ do
    char '.'
    lexeme nat
  char_ '?'
  pure $ ConditionalDef ident n

resultType :: Parser ResultType
resultType = try implicit <|> explicit
  where
    implicit = do
      ident <- boxedTypeIdent
      exprs <- many subExpr
      pure $ RTypeApp ident exprs
    explicit = do
      ident <- boxedTypeIdent
      char_ '<'
      exprs <- lexeme $ sepBy1 (lexeme subExpr) (char_ ',')
      char_ '>'
      pure $ RTypeApp ident $ toList exprs

--------------------------------------

partialAppDecl :: Parser PartialAppDecl
partialAppDecl = typeApp <|> combApp
  where
    typeApp = PartialTypeApp <$> partialTypeAppDecl
    combApp = PartialCombApp <$> partialCombAppDecl

partialTypeAppDecl :: Parser PartialTypeAppDecl
partialTypeAppDecl = try juxtaposition <|> explicit
  where
    juxtaposition = do
      ident <- boxedTypeIdent
      exprs <- some subExpr
      char_ ';'
      pure $ Juxtaposition ident exprs
    explicit = do
      ident <- boxedTypeIdent
      char_ '<'
      exprs <- lexeme $ sepBy1 (expr) (char ',')
      char_ '>'
      char_ ';'
      pure $ Explicit ident exprs

partialCombAppDecl :: Parser PartialCombAppDecl
partialCombAppDecl = do
  ident <- combIdent
  exprs <- some subExpr
  pure $ PartialCombAppDecl ident exprs

-----------------------------

finalDecl :: Parser FinalDecl
finalDecl = try new <|> try final <|> empty
  where
    new =
      New
        <$> between
          newKw
          (char_ ';')
          (lexeme boxedTypeIdent)
    final =
      Final
        <$> between
          finalKw
          (char_ ';')
          (lexeme boxedTypeIdent)
    empty =
      Empty
        <$> between
          emptyKw
          (char_ ';')
          (lexeme boxedTypeIdent)
