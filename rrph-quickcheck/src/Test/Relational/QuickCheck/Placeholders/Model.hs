{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DataKinds, DeriveGeneric #-}

module Test.Relational.QuickCheck.Placeholders.Model (
  A (..), a0', a1', a2', relA,
  B (..), b0', b1', b2', relB,
  Ph (..), ph0', ph1', ph2',
  ) where

import GHC.Generics (Generic)
import Data.Int (Int64)
import qualified Database.Relational.Table as Table
import Database.Relational (Relation, table, TableDerivable (derivedTable))
import Database.HDBC.Query.TH (makeRelationalRecord)


data A =
  A
  { a0 :: Int64
  , a1 :: Int64
  , a2 :: Int64
  } deriving (Eq, Ord, Show, Generic)

data B =
  B
  { b0 :: Int64
  , b1 :: Int64
  , b2 :: Int64
  } deriving (Eq, Ord, Show, Generic)

data Ph =
  Ph
  { ph0 :: Int64
  , ph1 :: Int64
  , ph2 :: Int64
  } deriving (Eq, Ord, Show, Generic)


$(makeRelationalRecord ''A)
$(makeRelationalRecord ''B)
$(makeRelationalRecord ''Ph)

instance TableDerivable A where
  derivedTable = Table.table "ARBITRARY0.A" ["a0", "a1", "a2"]

relA :: Relation () A
relA = table derivedTable

instance TableDerivable B where
  derivedTable = Table.table "ARBITRARY0.B" ["b0", "b1", "b2"]

relB :: Relation () B
relB = table derivedTable
