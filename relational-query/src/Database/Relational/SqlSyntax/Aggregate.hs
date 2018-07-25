-- |
-- Module      : Database.Relational.SqlSyntax.Aggregate
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides grouping-sets structure of SQL syntax tree.
module Database.Relational.SqlSyntax.Aggregate (
  aggregateColumnRef,
  aggregatePowerKey, aggregateGroupingSet,
  aggregateRollup, aggregateCube, aggregateSets,

  aggregateKeyRecord, aggregateKeyElement, unsafeAggregateKey,
  ) where

import Database.Relational.SqlSyntax.Types
  (AggregateBitKey (..), AggregateSet (..), AggregateElem (..),
   AggregateColumnRef, AggregateKey (..), )


-- | Single term aggregation element.
aggregateColumnRef :: AggregateColumnRef i j -> AggregateElem i j
aggregateColumnRef =  ColumnRef

-- | Key of aggregation power set.
aggregatePowerKey :: [AggregateColumnRef i j] -> AggregateBitKey i j
aggregatePowerKey =  AggregateBitKey

-- | Single grouping set.
aggregateGroupingSet :: [AggregateElem i j] -> AggregateSet i j
aggregateGroupingSet =  AggregateSet

-- | Rollup aggregation element.
aggregateRollup :: [AggregateBitKey i j] -> AggregateElem i j
aggregateRollup =  Rollup

-- | Cube aggregation element.
aggregateCube :: [AggregateBitKey i j] -> AggregateElem i j
aggregateCube =  Cube

-- | Grouping sets aggregation.
aggregateSets :: [AggregateSet i j] -> AggregateElem i j
aggregateSets =  GroupingSets

-- | Extract typed record from 'AggregateKey'.
aggregateKeyRecord :: AggregateKey i j a -> a
aggregateKeyRecord (AggregateKey (p, _c)) = p

-- | Extract untyped term from 'AggregateKey'.
aggregateKeyElement :: AggregateKey i j a -> AggregateElem i j
aggregateKeyElement (AggregateKey (_p, c)) = c

-- | Unsafely bind typed-record and untyped-term into 'AggregateKey'.
unsafeAggregateKey :: (a, AggregateElem i j) -> AggregateKey i j a
unsafeAggregateKey = AggregateKey
