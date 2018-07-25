{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, KindSignatures #-}

-- |
-- Module      : Database.Relational.SqlSyntax.Types
-- Copyright   : 2015-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.SqlSyntax.Types (
  -- * The SubQuery
  SubQuery (..),

  -- * Set operations
  Duplication (..), SetOp (..), BinOp (..),

  -- * Qualifiers for nested query
  Qualifier (..), Qualified (..), qualifier, unQualify, qualify,

  -- * Ordering types
  Order (..), Nulls (..), OrderColumn, OrderingTerm,

  -- * Aggregating types
  AggregateColumnRef,
  AggregateBitKey (..), AggregateSet (..), AggregateElem (..),

  AggregateKey (..),

  -- * Product tree type
  NodeAttr (..), ProductTree (..),
  Node (..), nodeAttr, nodeTree,
  JoinProduct,

  -- * Case
  CaseClause (..), WhenClauses(..),

  -- * Column, Tuple, Record and Projection
  Column (..), Tuple, tupleWidth,
  Record, untypeRecord, record, PI,
  recordWidth,
  typeFromRawColumns,
  typeFromScalarSubQuery,

  -- * Predicate to restrict Query result
  Predicate,
  )  where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Internal.String (StringSQL)
import Database.Relational.Internal.UntypedTable (Untyped)


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Set operators
data SetOp = Union | Except | Intersect  deriving Show

-- | Set binary operators
newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Order of null.
data Nulls =  NullsFirst | NullsLast deriving Show

-- | Type for order-by column
type OrderColumn i j = Column i j

-- | Type for order-by term
type OrderingTerm i j = ((Order, Maybe Nulls), OrderColumn i j)

-- | Type for group-by term
type AggregateColumnRef i j = Column i j

-- | Type for group key.
newtype AggregateBitKey i j = AggregateBitKey [AggregateColumnRef i j] deriving Show

-- | Type for grouping set
newtype AggregateSet i j = AggregateSet [AggregateElem i j] deriving Show

-- | Type for group-by tree
data AggregateElem i j = ColumnRef (AggregateColumnRef i j)
                   | Rollup [AggregateBitKey i j]
                   | Cube   [AggregateBitKey i j]
                   | GroupingSets [AggregateSet i j]
                   deriving Show

-- | Typeful aggregate element.
newtype AggregateKey i j a = AggregateKey (a, AggregateElem i j)

-- | Sub-query type
data SubQuery i j = Table Untyped
              | Flat Config
                (Tuple i j) Duplication (JoinProduct i j) [Predicate i j Flat]
                [OrderingTerm i j]
              | Aggregated Config
                (Tuple i j) Duplication (JoinProduct i j) [Predicate i j Flat]
                [AggregateElem i j] [Predicate i j Aggregated] [OrderingTerm i j]
              | Bin BinOp (SubQuery i j) (SubQuery i j)
              deriving Show

-- | Qualifier type.
newtype Qualifier = Qualifier Int  deriving Show

-- | Qualified query.
data Qualified a =
  Qualified Qualifier a
  deriving (Show, Functor, Foldable, Traversable)

-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier (Qualified q _) = q

-- | Unqualify.
unQualify :: Qualified a -> a
unQualify (Qualified _ a) = a

-- | Add qualifier
qualify :: Qualifier -> a -> Qualified a
qualify = Qualified


-- | node attribute for product.
data NodeAttr = Just' | Maybe deriving Show

type QS i j = Qualified (SubQuery i j)

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree i j rs
  = Leaf (QS i j)
  | Join !(Node i j rs) !(Node i j rs) !rs
  deriving (Show, Functor)

-- | Product node. node attribute and product tree.
data Node i j rs = Node !NodeAttr !(ProductTree i j rs)  deriving (Show, Functor)

-- | Get node attribute.
nodeAttr :: Node i j rs -> NodeAttr
nodeAttr (Node a _) = a

-- | Get tree from node.
nodeTree :: Node i j rs -> ProductTree i j rs
nodeTree (Node _ t) = t

-- | Type for join product of query.
type JoinProduct i j = Maybe (ProductTree i j [Predicate i j Flat])

-- | when clauses
data WhenClauses i j =
  WhenClauses [(Tuple i j, Tuple i j)] (Tuple i j)
  deriving Show

-- | case clause
data CaseClause i j
  = CaseSearch (WhenClauses i j)
  | CaseSimple (Tuple i j) (WhenClauses i j)
  deriving Show

-- | Projected column structure unit with single column width
data Column i j
  = RawColumn StringSQL            -- ^ used in immediate value or unsafe operations
  | SubQueryRef (Qualified Int)    -- ^ normalized sub-query reference T<n> with Int index
  | Scalar (SubQuery i j)            -- ^ scalar sub-query
  | Case (CaseClause i j) Int            -- ^ <n>th column of case clause
  deriving Show

-- | Untyped projected tuple. Forgot record type.
type Tuple i j = [Column i j]

-- | Width of 'Tuple'.
tupleWidth :: Tuple i j -> Int
tupleWidth = length

-- | Phantom typed record. Projected into Haskell record type 't'.
newtype Record i j c t =
  Record
  { untypeRecord :: Tuple i j }  deriving Show

-- | Type for predicate to restrict of query result.
type Predicate i j c = Record i j c (Maybe Bool)

-- | Type for projection function.
type PI i j c a b = Record i j c a -> Record i j c b

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: Tuple i j -> Record i j c t
record = Record

-- | Width of 'Record'.
recordWidth :: Record i j c r -> Int
recordWidth = length . untypeRecord

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: [StringSQL] -- ^ SQL string list specifies columns
                   -> Record i j c r  -- ^ Result 'Record'
typeFromRawColumns =  record . map RawColumn

-- | Unsafely generate 'Record' from scalar sub-query.
typeFromScalarSubQuery :: SubQuery i j -> Record i j c t
typeFromScalarSubQuery = record . (:[]) . Scalar
