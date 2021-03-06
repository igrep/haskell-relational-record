{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Assigning
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from context into context with assigning.
module Database.Relational.Query.Monad.Trans.Assigning (
  -- * Transformer into context with assignments
  Assignings, assignings,

  -- * API of context with assignments
  assignTo, (<-#), AssignTarget,

  -- * Result SQL set clause
  extractAssignments
  ) where

import Database.Relational.Query.Context (Flat)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.Monoid (mconcat)
import Data.DList (DList, toList)

import Database.Relational.Query.Component (Assignment, Assignments)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class (MonadRestrict(..))


-- | Type to accumulate assigning context.
--   Type 'r' is table record type.
newtype Assignings r m a =
  Assignings (WriterT (Table r -> DList Assignment) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Assignings'
assignings :: Monad m => m a -> Assignings r m a
assignings =  lift

-- | 'MonadRestrict' with ordering.
instance MonadRestrict c m => MonadRestrict c (Assignings r m) where
  restrict = assignings . restrict

-- | Target of assignment.
type AssignTarget r v = Pi r v

targetProjection :: AssignTarget r v ->  Table r -> Projection Flat v
targetProjection pi' tbl = Projection.pi (Projection.unsafeFromTable tbl) pi'

-- | Add an assignment.
assignTo :: Monad m => Projection Flat v ->  AssignTarget r v -> Assignings r m ()
assignTo vp target = Assignings . tell
                     $ \t -> mconcat $ zipWith (curry pure) (leftsR t) rights  where
  leftsR = Projection.columns . targetProjection target
  rights = Projection.columns vp

-- | Add and assginment.
(<-#) :: Monad m => AssignTarget r v -> Projection Flat v -> Assignings r m ()
(<-#) =  flip assignTo

infix 4 <-#

-- | Run 'Assignings' to get 'Assignments'
extractAssignments :: (Monad m, Functor m)
                   => Assignings r m a
                   -> m (a, Table r -> Assignments)
extractAssignments (Assignings ac) = second (toList .) <$> runWriterT ac
