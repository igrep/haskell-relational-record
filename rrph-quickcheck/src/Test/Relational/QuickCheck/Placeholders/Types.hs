{-# LANGUAGE TemplateHaskell #-}

-- dragenArbitrary defines orphan instances for Flat and Aggregated
{-# OPTIONS_GHC -Wno-orphans #-}

-- dragenArbitrary defines unused argument when definining (Arbitrary Flat) and (Arbitrary Aggregated)
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.Relational.QuickCheck.Placeholders.Types where

import Data.Int (Int64)
import Dragen (dragenArbitrary, uniform, weighted)
import Database.Relational

import Test.Relational.QuickCheck.Placeholders.Model

type RelationAB = Either (Relation () A) (Relation () B)

type RecordAB c = Either (Record c A) (Record c B)

data ReturnVal
  = ReturnPh
  | ReturnSelected
  deriving Show

$(dragenArbitrary ''ReturnVal 1 uniform)

data Op
  = Plus
  | Minus
  deriving (Eq, Ord)

$(dragenArbitrary ''Op 1 uniform)

instance Show Op where
  show Plus  = ".+."
  show Minus = ".-."

-- | Integer expression which has at least one record selector expression.
data VarExpr c
  = Column
  | Placeholder
  | VLeft  (VarExpr c) Op Int64
  | VRight Int64 Op (VarExpr c)

-- dragenArbitrary only accepts a type without type arguments.
type VarExprFlat = VarExpr Flat
type VarExprAggregated = VarExpr Aggregated

$(dragenArbitrary ''VarExprFlat 2 $ weighted [('Column, 2), ('Placeholder, 4), ('VLeft, 1), ('VRight, 1)])
$(dragenArbitrary ''VarExprAggregated 2 $ weighted [('Column, 2), ('Placeholder, 4), ('VLeft, 1), ('VRight, 1)])

instance Show (VarExpr c) where
  show Column = "tb ! col"
  show Placeholder = "ph ! col"
  show (VLeft  e o v) = "(" ++ unwords [show e, show o, show v] ++ ")"
  show (VRight v o e) = "(" ++ unwords [show v, show o, show e] ++ ")"


data Expr c
  = Var (VarExpr c)
  | Expr c :+: Expr c
  | Expr c :-: Expr c

type ExprFlat = Expr Flat
type ExprAggregated = Expr Aggregated

$(dragenArbitrary ''ExprFlat 2 $ weighted [('Var, 4), ('(:+:), 1), ('(:-:), 1)])
$(dragenArbitrary ''ExprAggregated 2 $ weighted [('Var, 4), ('(:+:), 1), ('(:-:), 1)])

instance Show (Expr c) where
  show (Var ve) = show ve
  show (e1 :+: e2) = "(" ++ unwords [show e1, ".+.", show e2]
  show (e1 :-: e2) = "(" ++ unwords [show e1, ".-.", show e2]

data Cmp
  = Lt
  | Eq
  | Gt
  deriving (Eq, Ord)

$(dragenArbitrary ''Cmp 1 uniform)

instance Show Cmp where
  show Lt = ".<."
  show Eq = ".=."
  show Gt = ".>."

newtype Term c =
  Term (Expr c, Cmp, Expr c)

type TermFlat = Term Flat
type TermAggregated = Term Aggregated

$(dragenArbitrary ''TermFlat 1 uniform)
$(dragenArbitrary ''TermAggregated 1 uniform)

instance Show (Term c) where
  show (Term (e1, c, e2)) = "(" ++ unwords [show e1, show c, show e2] ++ ")"

data Pred c
  = PTerm (Term c)
  | Not (Pred c)
  | Pred c :&: Pred c
  | Pred c :|: Pred c

type PredFlat = Pred Flat
type PredAggregated = Pred Aggregated

$(dragenArbitrary ''PredFlat 2 $ weighted [('PTerm, 2), ('Not, 1), ('(:&:), 1), ('(:|:), 1)])
$(dragenArbitrary ''PredAggregated 2 $ weighted [('PTerm, 2), ('Not, 1), ('(:&:), 1), ('(:|:), 1)])

instance Show (Pred c) where
  show (PTerm t) = show t
  show (Not p) = "not' (" ++ show p ++ ") "
  show (p1 :&: p2) = "(" ++ show p1 ++ " and' " ++ show p2 ++ ")"
  show (p1 :|: p2) = "(" ++ show p1 ++ " or' " ++ show p2 ++ ")"

type AggregatingPowerSetExpr = (Expr Flat, Expr Flat)

type AggregatingSetListExpr = (AggregatingSetExpr, AggregatingSetExpr)

data AggregatingSetExpr =
  AgKey (Expr Flat) | AgKey' AgExpr

instance Show AggregatingSetExpr where
  show (AgKey expr) = "key " ++ show expr
  show (AgKey' aexp) = "key' " ++ show aexp

data AgExpr =
  Rollup AggregatingPowerSetExpr
  | Cube AggregatingPowerSetExpr
  | GroupingSets AggregatingSetListExpr

instance Show AgExpr where
  show (Rollup pse) = "rollup " ++ show pse
  show (Cube pse) = "cube " ++ show pse
  show (GroupingSets sle) = "groupingSets " ++ show sle

$(dragenArbitrary ''AgExpr 2 $ weighted [('Rollup, 2), ('Cube, 2), ('GroupingSets, 1)])
$(dragenArbitrary ''AggregatingSetExpr 2 $ weighted [('AgKey, 4), ('AgKey', 1)])

data CommonAction
  = AQuery
  | AOn (Pred Flat)
  | AWheres (Pred Flat)

$(dragenArbitrary ''CommonAction 1 uniform)

instance Show CommonAction where
  show AQuery = "query"
  show (AOn pd) = "on (" ++ show pd ++ ")"
  show (AWheres pd) = "wheres (" ++ show pd ++ ")"

data SimpleAction
  = SimpleCommon CommonAction
  | AFlatOrderBy (Expr Flat)

$(dragenArbitrary ''SimpleAction 1 uniform)

instance Show SimpleAction where
  show (SimpleCommon c) = show c
  show (AFlatOrderBy expr) = "asc (" ++ show expr ++ ")"

data AggregatedAction
  = AggregatedCommon CommonAction
  | AGroupBy (Expr Flat)
  | AGroupBy' AgExpr
  | AHaving (Pred Aggregated)
  | AAggregatedOrderBy (Expr Aggregated)

$(dragenArbitrary ''AggregatedAction 1 uniform)

instance Show AggregatedAction where
  show (AggregatedCommon c) = show c
  show (AGroupBy expr) = "groupBy (" ++ show expr ++ ")"
  show (AGroupBy' expr) = "groupBy' (" ++ show expr ++ ")"
  show (AHaving pd) = "having (" ++ show pd ++ ")"
  show (AAggregatedOrderBy expr) = "asc (" ++ show expr ++ ")"
