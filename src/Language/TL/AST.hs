{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

-- | stripped down tl AST adapted for td_api.tl
module Language.TL.AST where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics
import qualified Language.TL.Types as T

-- | documentation
type Ann = Maybe Text

type Doc = Map Text Text

class ToTerm a where
  toTerm :: a -> Term

class ToType a where
  toType :: a -> Type

id2t :: T.Ident -> Text
id2t = T.ident

fid2t :: T.FullIdent -> Text
fid2t (T.FullName ident _) = id2t ident

instance ToType T.Ident where
  toType = Type . T.ident

instance ToTerm T.Ident where
  toTerm = Var . T.ident

instance ToType T.TypeIdent where
  toType (T.Boxed ty) = toType ty
  toType (T.LcIdent ident) = toType ident
  toType T.NatType = NatType

instance ToTerm T.Term where
  toTerm (T.Expr e) = toTerm e
  toTerm (T.Type tyIdent) = error "Trying to convert a type into a term"
  toTerm (T.Var ident) = toTerm ident
  toTerm (T.Nat i) = Nat i
  toTerm (T.PTerm t) = toTerm t
  toTerm (T.TypeApp ident exprs') =
    error "Trying to convert a type application into a term"

instance ToType T.Term where
  toType (T.Expr e) = toType e
  toType (T.Type tyIdent) = toType tyIdent
  toType (T.Var _) = error "Trying to convert a var into a type"
  toType (T.Nat _) = error "Trying to convert a nat into a type"
  toType (T.PTerm t) = toType t
  toType (T.TypeApp tyIdent l) =
    let exprs = toList l
     in TypeApp (toType tyIdent) (fmap toType exprs)

instance ToTerm T.SubExpr where
  toTerm (T.Sum ((Right t) :| [])) = toTerm t
  toTerm _ = error "subExpr is not a term"

instance ToType T.SubExpr where
  toType (T.Sum ((Right t) :| [])) = toType t
  toType _ = error "subExpr is not a term"

instance ToTerm T.Expr where
  toTerm (T.Exprs exprs) =
    let h = head exprs
        t = tail exprs
     in App (toTerm h) (fmap toTerm t)

instance ToType T.Expr where
  toType (T.Exprs subexprs) =
    let h = head subexprs
        t = tail subexprs
     in TypeApp (toType h) (fmap toType t)

instance ToType T.ResultType where
  toType (T.RTypeApp ident subExprs) =
    if null subExprs
      then toType ident
      else TypeApp (toType ident) (fmap toType subExprs)

combConv :: Doc -> T.CombinatorDecl -> Combinator
combConv doc T.CombinatorDecl {..} =
  if null optArglist
    then case combId of
      T.Optional fid ->
        let ident = fid2t fid
         in Combinator
              { args = argsList >>= argsConv doc,
                resType = toType resType,
                ann = M.lookup "description" doc,
                ..
              }
      T.Omitted -> error "top level combinator with omitted "
    else error ""
combConv _ T.BuiltinDecl {} = error "builtin decl"

argsConv :: Doc -> T.Args -> [Arg]
argsConv doc (T.Named (T.Optional id) _ _ t) =
  let ident = id2t id
   in pure $ Arg ident (M.lookup ident doc) (toType t)
argsConv _ T.Named {} = error "unnamed argument"
argsConv _ T.MultipleArgs {} = error "multiplicity"
argsConv doc (T.NamedList l _ t) =
  let ids = toList l
   in fmap
        ( \case
            T.Optional id ->
              let ident = id2t id
               in Arg ident (M.lookup ident doc) (toType t)
            _ -> error "uunamed argument"
        )
        ids
argsConv _ T.Unnamed {} = error "unnamed argument"

data Combinator
  = Combinator
      { ident :: Text,
        ann :: Maybe Text,
        args :: [Arg],
        resType :: Type
      }
  deriving (Show, Eq, Generic)

combArity :: Combinator -> Int
combArity Combinator {..} =
  case resType of
    Type _ -> 0
    NatType -> error "result type is NatType"
    TypeApp _ param -> length param

combName :: Combinator -> Text
combName Combinator {..} =
  case resType of
    Type t -> t
    NatType -> error "result type is NatType"
    TypeApp (Type t) _ -> t
    TypeApp _ _ -> error "combinator is not an ADT"

data ADT
  = ADT
      { name :: Text,
        ann :: Ann,
        constructors :: [Combinator]
      }
  deriving (Show, Eq, Generic)

newtype Function
  = Function Combinator
  deriving (Show, Eq, Generic)

data Type
  = Type Text
  | TypeApp Type [Type]
  | NatType
  deriving (Show, Eq, Generic)

data Term
  = Var Text
  | Nat Int
  | App Term [Term]
  deriving (Show, Eq, Generic)

data Arg
  = Arg
      { argName :: Text,
        ann :: Ann,
        argType :: Type
      }
  deriving (Show, Eq, Generic)
