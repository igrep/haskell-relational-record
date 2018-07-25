-- |
-- Module      : Database.Relational.SqlSyntax.Query
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides building and expanding operations of SQL query tree.
module Database.Relational.SqlSyntax.Query (
  flatSubQuery, aggregatedSubQuery,
  union, except, intersect,
  caseSearch, case',
  ) where

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.SqlSyntax.Types
  (Duplication (..), SetOp (..), BinOp (..),
   OrderingTerm, AggregateElem,
   JoinProduct, Predicate, WhenClauses (..), CaseClause (..), SubQuery (..),
   Column (..), Tuple, Record, record, untypeRecord, recordWidth, )


-- | Unsafely generate flat 'SubQuery' from untyped components.
flatSubQuery :: Config
             -> Tuple i j
             -> Duplication
             -> JoinProduct i j
             -> [Predicate i j Flat]
             -> [OrderingTerm i j]
             -> SubQuery i j
flatSubQuery = Flat

-- | Unsafely generate aggregated 'SubQuery' from untyped components.
aggregatedSubQuery :: Config
                   -> Tuple i j
                   -> Duplication
                   -> JoinProduct i j
                   -> [Predicate i j Flat]
                   -> [AggregateElem i j]
                   -> [Predicate i j Aggregated]
                   -> [OrderingTerm i j]
                   -> SubQuery i j
aggregatedSubQuery = Aggregated

setBin :: SetOp -> Duplication -> SubQuery i j -> SubQuery i j -> SubQuery i j
setBin op = Bin . BinOp . (,) op

-- | Union binary operator on 'SubQuery'
union     :: Duplication -> SubQuery i j -> SubQuery i j -> SubQuery i j
union     =  setBin Union

-- | Except binary operator on 'SubQuery'
except    :: Duplication -> SubQuery i j -> SubQuery i j -> SubQuery i j
except    =  setBin Except

-- | Intersect binary operator on 'SubQuery'
intersect :: Duplication -> SubQuery i j -> SubQuery i j -> SubQuery i j
intersect =  setBin Intersect


whenClauses :: String                     -- ^ Error tag
            -> [(Record i j c a, Record i j c b)] -- ^ Each when clauses
            -> Record i j c b                 -- ^ Else result record
            -> WhenClauses i j              -- ^ Result clause
whenClauses eTag ws0 e = d ws0
  where
    d []       = error $ eTag ++ ": Empty when clauses!"
    d ws@(_:_) =
      WhenClauses [ (untypeRecord p, untypeRecord r) | (p, r) <- ws ]
      $ untypeRecord e

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: [(Predicate i j c, Record i j c a)] -- ^ Each when clauses
           -> Record i j c a                  -- ^ Else result record
           -> Record i j c a                  -- ^ Result record
caseSearch ws e =
    record [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSearch $ whenClauses "caseSearch" ws e

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: Record i j c a                 -- ^ Record value to match
      -> [(Record i j c a, Record i j c b)] -- ^ Each when clauses
      -> Record i j c b                 -- ^ Else result record
      -> Record i j c b                 -- ^ Result record
case' v ws e =
    record [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSimple (untypeRecord v) $ whenClauses "case'" ws e
