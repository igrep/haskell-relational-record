-- |
-- Module      : Database.Relational.Monad.Trans.JoinState
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides state definition for
-- "Database.Relational.Monad.Trans.Join".
--
-- This is not public interface.
module Database.Relational.Monad.Trans.JoinState (
  -- * Join context
  JoinContext, primeJoinContext, updateProduct, joinProduct
  ) where

import Prelude hiding (product)
import Data.DList (DList, toList)

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax (JoinProduct, Node, Predicate)
import qualified Database.Relational.SqlSyntax as Product


-- | JoinContext type for QueryJoin.
newtype JoinContext i j =
  JoinContext
  { product  :: Maybe (Node i j (DList (Predicate i j Flat)))
  }

-- | Initial 'JoinContext'.
primeJoinContext :: JoinContext i j
primeJoinContext =  JoinContext Nothing

-- | Update product of 'JoinContext'.
updateProduct :: (Maybe (Node i j (DList (Predicate i j Flat))) -> Node i j (DList (Predicate i j Flat)))
              -> JoinContext i j
              -> JoinContext i j
updateProduct uf ctx = ctx { product = Just . uf . product $ ctx }

-- |  Finalize context to extract accumulated query product.
joinProduct :: JoinContext i j -> JoinProduct i j
joinProduct =  fmap (fmap toList . Product.nodeTree) . product
