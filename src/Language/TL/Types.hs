{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module Language.TL.Types where

import Control.DeepSeq
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import GHC.Generics

data Case
  = U
  | L
  deriving (Show, Eq, Enum, Generic, NFData)

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Generic, NFData)

data Optional c
  = Optional c
  | Omitted
  deriving (Show, Eq, Generic, NFData)

data FullIdent
  = FullName Ident (Maybe Int)
  deriving (Show, Eq, Generic, NFData)

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
  deriving (Show, Eq, Generic, NFData)

data DeclBlock
  = FunDeclBlk [AnnDecl]
  | TypeDeclBlk [AnnDecl]
  deriving (Show, Eq, Generic, NFData)

type Program = [DeclBlock]

data AnnDecl
  = AnnDecl [Comment] Decl
  deriving (Show, Eq, Generic, NFData)

data Decl
  = Combinator CombinatorDecl
  | PartialApp PartialAppDecl
  | FinalDecl FinalDecl
  deriving (Show, Eq, Generic, NFData)

newtype Expr
  = Exprs
      { subexprs :: [SubExpr]
      }
  deriving (Show, Eq, Generic, NFData)

data SubExpr
  = Sum (NonEmpty (Either Int Term))
  deriving (Show, Eq, Generic, NFData)

data Term
  = Expr Expr
  | Type TypeIdent
  | Var Ident
  | Nat Int
  | PTerm Term
  | TypeApp TypeIdent (NonEmpty Expr)
  deriving (Show, Eq, Generic, NFData)

data TypeIdent
  = Boxed BoxedTypeIdent
  | LcIdent Ident
  | NatType
  deriving (Show, Eq, Generic, NFData)

type BoxedTypeIdent = Ident

data VarIdent
  = VarIdent Text
  deriving (Show, Eq, Generic, NFData)

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
  deriving (Show, Eq, Generic, NFData)

data OptArgs
  = OptArgs (NonEmpty Ident) Bool Expr
  deriving (Show, Eq, Generic, NFData)

data Args
  = Named (Optional Ident) (Maybe ConditionalDef) Bool (TypeTerm)
  | MultipleArgs (Maybe (Optional Ident)) (Maybe NatTerm) [Args]
  | NamedList (NonEmpty (Optional Ident)) Bool TypeTerm
  | Unnamed Bool TypeTerm
  deriving (Show, Eq, Generic, NFData)

data ConditionalDef
  = ConditionalDef Ident (Maybe Int)
  deriving (Show, Eq, Generic, NFData)

data ResultType
  = RTypeApp BoxedTypeIdent [SubExpr]
  deriving (Show, Eq, Generic, NFData)

data PartialAppDecl
  = PartialTypeApp PartialTypeAppDecl
  | PartialCombApp PartialCombAppDecl
  deriving (Show, Eq, Generic, NFData)

data PartialTypeAppDecl
  = Juxtaposition BoxedTypeIdent (NonEmpty SubExpr)
  | Explicit BoxedTypeIdent (NonEmpty Expr)
  deriving (Show, Eq, Generic, NFData)

data PartialCombAppDecl
  = PartialCombAppDecl (Optional Ident) (NonEmpty SubExpr)
  deriving (Show, Eq, Generic, NFData)

data FinalDecl
  = New BoxedTypeIdent
  | Final BoxedTypeIdent
  | Empty BoxedTypeIdent
  deriving (Show, Eq, Generic, NFData)
