{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module Language.TL.Types where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

data Case
  = U
  | L
  deriving (Show, Eq, Enum)

data Comment
  = LineComment Text
  | BlockComment Text
  deriving stock (Show, Eq)

data Optional c
  = Optional c
  | Omitted
  deriving stock (Show, Eq)

data FullIdent
  = FullName Ident (Maybe Int)
  deriving stock (Show, Eq)

data Ident
  = Qualified
      { casing :: Case,
        ns :: Text,
        ident :: Text
      }
  | Unqualified
      { casing :: Case,
        ident :: Text
      }
  deriving stock (Show, Eq)

data DeclBlock
  = FunDeclBlk [AnnDecl]
  | TypeDeclBlk [AnnDecl]
  deriving stock (Show, Eq)

type Program = [DeclBlock]

data AnnDecl
  = AnnDecl [Comment] Decl
  deriving stock (Show, Eq)

data Decl
  = Combinator CombinatorDecl
  | PartialApp PartialAppDecl
  | FinalDecl FinalDecl
  deriving stock (Show, Eq)

newtype Expr = Exprs
  { subexprs :: [SubExpr]
  }
  deriving stock (Show, Eq)

newtype SubExpr
  = Sum (NonEmpty (Either Int Term))
  deriving stock (Show, Eq)

data Term
  = Expr Expr
  | Type TypeIdent
  | Var Ident
  | Nat Int
  | PTerm Term
  | TypeApp TypeIdent (NonEmpty Expr)
  deriving stock (Show, Eq)

data TypeIdent
  = Boxed BoxedTypeIdent
  | LcIdent Ident
  | NatType
  deriving stock (Show, Eq)

type BoxedTypeIdent = Ident

newtype VarIdent
  = VarIdent Text
  deriving stock (Show, Eq)

type TypeTerm = Term

type NatTerm = Term

data CombinatorDecl
  = CombinatorDecl
      { combId :: Optional FullIdent,
        optArglist :: [OptArgs],
        argsList :: [Args],
        resType :: ResultType
      }
  | BuiltinDecl
      { combId :: Optional FullIdent,
        builtinType :: BoxedTypeIdent
      }
  deriving stock (Show, Eq)

data OptArgs
  = OptArgs (NonEmpty Ident) Bool Expr
  deriving stock (Show, Eq)

data Args
  = Named (Optional Ident) (Maybe ConditionalDef) Bool TypeTerm
  | MultipleArgs (Maybe (Optional Ident)) (Maybe NatTerm) [Args]
  | NamedList (NonEmpty (Optional Ident)) Bool TypeTerm
  | Unnamed Bool TypeTerm
  deriving stock (Show, Eq)

data ConditionalDef
  = ConditionalDef Ident (Maybe Int)
  deriving stock (Show, Eq)

data ResultType
  = RTypeApp BoxedTypeIdent [SubExpr]
  deriving stock (Show, Eq)

data PartialAppDecl
  = PartialTypeApp PartialTypeAppDecl
  | PartialCombApp PartialCombAppDecl
  deriving stock (Show, Eq)

data PartialTypeAppDecl
  = Juxtaposition BoxedTypeIdent (NonEmpty SubExpr)
  | Explicit BoxedTypeIdent (NonEmpty Expr)
  deriving stock (Show, Eq)

data PartialCombAppDecl
  = PartialCombAppDecl (Optional Ident) (NonEmpty SubExpr)
  deriving stock (Show, Eq)

data FinalDecl
  = New BoxedTypeIdent
  | Final BoxedTypeIdent
  | Empty BoxedTypeIdent
  deriving stock (Show, Eq)
