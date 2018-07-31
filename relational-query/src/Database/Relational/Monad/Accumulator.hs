{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeOperators      #-}

module Database.Relational.Monad.Accumulator where

import           Database.Relational.Config
import           Database.Relational.Monad.Trans.JoinState
import           Database.Relational.SqlSyntax.Types       hiding (Table)
import           Database.Relational.SqlSyntax.Updates
import           Database.Relational.Table

import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Data.DList                                (DList)
import           Data.Extensible                           (type (>:))
import qualified Data.Extensible                           as E
import           Data.Monoid                               (Last)


type Accumulator c at r i j = E.Record
 '[ "aggregatingsTerm" >: DList at
  , "tableAssignments" >: (Table r -> DList Assignment)
  , "queryConfig" >: Config
  , "joinContext" >: JoinContext i j
  , "lastDuplication" >: Last Duplication
  , "orderingTerms" >: DList (OrderingTerm i j)
  , "qualificationCount" >: Int
  , "restrictings" >: DList (Predicate i j c)
  , "placeHolderValues" >: DList String
  ]


newtype AccumulatorM c at r i j a =
  AccumulatorM (IxState (Accumulator c at r i i) (Accumulator c at r j j) a)
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadState)
