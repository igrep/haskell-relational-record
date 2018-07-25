{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Restricting
-- Copyright   : 2014-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift to basic 'MonadQuery'.
module Database.Relational.Monad.Trans.Restricting (
  -- * Transformer into restricted context
  Restrictings, restrictings,

  -- * Result
  extractRestrict
  ) where

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Database.Relational.SqlSyntax (Predicate)

import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery (..), MonadAggregate(..))


-- igrep NOTE: Convert this into IxMonad or else?

-- | Type to accumulate query restrictions.
--   Type 'c' is context tag of restriction building like
--   Flat (where) or Aggregated (having).
newtype Restrictings c m a =
  Restrictings (WriterT (DList (Predicate i j c)) m a)

instance IxFunctor (Restrictings c m) where
  imap = undefined

instance IxPointed (Restrictings c m) where
  ireturn = undefined

instance IxApplicative (Restrictings c m) where
  iap = undefined

instance IxMonadTrans (Restrictings c) where
  ilift = undefined

-- | Lift to 'Restrictings'
restrictings :: Monad m => m a -> Restrictings c m a
restrictings =  ilift

-- | Add whole query restriction.
updateRestriction :: Monad m => Predicate i j c -> Restrictings c m ()
updateRestriction =  Restrictings . tell . pure

-- | 'MonadRestrict' instance.
instance (Monad q, Functor q) => MonadRestrict c (Restrictings c q) i j where
  restrict = updateRestriction

-- | Restricted 'MonadQualify' instance.
instance MonadQualify q m => MonadQualify q (Restrictings c m) where
  liftQualify = restrictings . liftQualify

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (Restrictings c q) where
  setDuplication     = restrictings . setDuplication
  restrictJoin       = restrictings . restrictJoin
  query'             = restrictings . query'
  queryMaybe'        = restrictings . queryMaybe'

-- | Resticted 'MonadAggregate' instance.
instance MonadAggregate m => MonadAggregate (Restrictings c m) where
  groupBy  = restrictings . groupBy
  groupBy' = restrictings . groupBy'

-- | Run 'Restrictings' to get 'QueryRestriction'
extractRestrict :: (Monad m, Functor m) => Restrictings c m a -> m (a, [Predicate i j c])
extractRestrict (Restrictings rc) = second toList <$> runWriterT rc
