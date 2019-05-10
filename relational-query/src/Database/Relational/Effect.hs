-- |
-- Module      : Database.Relational.Effect
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines effect statements
-- like update and delete.
module Database.Relational.Effect (
  -- * Object to express simple restriction.
  Restriction, restriction, restriction',

  -- * Object to express update target columns and restriction.
  UpdateTarget, updateTarget, updateTarget',
  liftTargetAllColumn, liftTargetAllColumn',
  updateTargetAllColumn, updateTargetAllColumn',

  -- * Object to express insert terget.
  InsertTarget, insertTarget, insertTarget', piRegister,

  -- * Generate SQL from restriction.
  sqlWhereFromRestriction,
  sqlFromUpdateTarget,
  sqlChunkFromInsertTarget,
  sqlFromInsertTarget,
  sqlChunksFromRecordList,
  ) where

import Data.Monoid ((<>))
import Data.List (unfoldr)

import Language.SQL.Keyword (Keyword(..))
import Database.Record.Persistable (PersistableWidth)

import Database.Relational.Internal.Config (Config (chunksInsertSize), defaultConfig)
import Database.Relational.Internal.ContextType (PureOperand)
import Database.Relational.Internal.String (stringSQL, showStringSQL)
import Database.Relational.SqlSyntax
  (Record, composeWhere, composeSets, composeChunkValuesWithColumns, composeValuesListWithColumns,
   SQLWithPlaceholderOffsets', detachPlaceholderOffsets)

import Database.Relational.Pi (Pi, id',)
import Database.Relational.TupleInstances (fst', snd')
import qualified Database.Relational.Pi.Unsafe as Pi
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import qualified Database.Relational.Record as Record
import Database.Relational.ProjectableClass (LiteralSQL)
import Database.Relational.Projectable (value,  (!))
import Database.Relational.Monad.BaseType (pwPlaceholders, defaultPlaceholders)
import Database.Relational.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Monad.Restrict (RestrictedStatement)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Assign (AssignStatement)
import qualified Database.Relational.Monad.Assign as Assign
import Database.Relational.Monad.Register (Register)
import qualified Database.Relational.Monad.Register as Register
import Database.Relational.Monad.Trans.ReadPlaceholders (runReadPlaceholders, readPlaceholders, askPlaceholders)


-- | Restriction type with place-holder parameter 'p' and projected record type 'r'.
newtype Restriction p r = Restriction (RestrictedStatement p r ())

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: RestrictedStatement () r () -> Restriction () r
restriction = Restriction

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: RestrictedStatement p r () -> Restriction p r
restriction' = Restriction

runRestriction :: Restriction p r
               -> RestrictedStatement p r ()
runRestriction (Restriction qf) = qf

-- | SQL WHERE clause 'StringSQL' string from 'Restriction'.
sqlWhereFromRestriction :: Config -> Record PureOperand p -> Table r -> Restriction p r -> SQLWithPlaceholderOffsets'
sqlWhereFromRestriction config phs tbl (Restriction q) = composeWhere <$> sequenceA rs
  where (_ph, rs) = Restrict.extract (runReadPlaceholders (q $ Record.unsafeFromTable tbl) phs) config

-- | Show where clause.
instance (PersistableWidth p, TableDerivable r) => Show (Restriction p r) where
  show = showStringSQL . detachPlaceholderOffsets . sqlWhereFromRestriction defaultConfig defaultPlaceholders derivedTable


-- | UpdateTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype UpdateTarget p r = UpdateTarget (AssignStatement p r ())

-- | Finalize 'Target' monad and generate 'UpdateTarget'.
updateTarget :: AssignStatement () r ()
             -> UpdateTarget () r
updateTarget =  UpdateTarget

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
updateTarget' :: AssignStatement p r ()
              -> UpdateTarget p r
updateTarget' = UpdateTarget

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all.
liftTargetAllColumn :: PersistableWidth r
                    => Restriction () r
                    -> UpdateTarget r r
liftTargetAllColumn rs = updateTarget' $ \proj -> do
  ph <- askPlaceholders
  readPlaceholders $ do
    id' <-# ph
    assignings $ runReadPlaceholders (runRestriction rs proj) Record.pempty
  return ()

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
liftTargetAllColumn' :: (PersistableWidth p, PersistableWidth r)
                     => Restriction p r
                     -> UpdateTarget (r, p) r
liftTargetAllColumn' rs = updateTarget' $ \proj -> do
  ph <- askPlaceholders
  readPlaceholders $ do
    id' <-# ph ! fst'
    assignings $ runReadPlaceholders (runRestriction rs proj) (ph ! snd')
  return ()

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all.
updateTargetAllColumn :: PersistableWidth r
                      => RestrictedStatement () r ()
                      -> UpdateTarget r r
updateTargetAllColumn = liftTargetAllColumn . restriction

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
updateTargetAllColumn' :: (PersistableWidth p, PersistableWidth r)
                       => RestrictedStatement p r ()
                       -> UpdateTarget (r, p) r
updateTargetAllColumn' = liftTargetAllColumn' . restriction'


-- | SQL SET clause and WHERE clause 'StringSQL' string from 'UpdateTarget'
sqlFromUpdateTarget :: Config -> Record PureOperand p -> Table r -> UpdateTarget p r -> SQLWithPlaceholderOffsets'
sqlFromUpdateTarget config phs tbl (UpdateTarget q) = (<>) <$> composeSets (asR tbl) <*> (composeWhere <$> sequenceA rs)
  where ((_ph, asR), rs) = Assign.extract (runReadPlaceholders (q $ Record.unsafeFromTable tbl) phs) config

instance (PersistableWidth p, TableDerivable r) => Show (UpdateTarget p r) where
  show = showStringSQL . detachPlaceholderOffsets . sqlFromUpdateTarget defaultConfig defaultPlaceholders derivedTable


-- | InsertTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype InsertTarget p r = InsertTarget (Register p r ())

-- | Finalize 'Register' monad and generate 'InsertTarget'.
insertTarget :: Register () r ()
             -> InsertTarget () r
insertTarget =  InsertTarget

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
insertTarget' :: Register p r ()
              -> InsertTarget p r
insertTarget' = InsertTarget

-- | parametalized 'Register' monad from 'Pi'
piRegister :: PersistableWidth r
           => Pi r r'
           -> Register r' r ()
piRegister pi' = readPlaceholders (pi' <-# pwPlaceholders (Pi.width' pi'))

sqlChunkFromInsertTarget' :: Config
                          -> Int
                          -> Record PureOperand p
                          -> Table r
                          -> InsertTarget p r
                          -> SQLWithPlaceholderOffsets'
sqlChunkFromInsertTarget' config sz phs tbl (InsertTarget q) =
    (\cs -> INSERT <> INTO <> stringSQL (Table.name tbl) <> cs) <$> composeChunkValuesWithColumns sz (asR tbl)
  where
    (_, asR) = Register.extract (runReadPlaceholders q phs) config

countChunks :: Config
            -> Table r
            -> Int
countChunks config tbl =
    (th + w - 1) `quot` w
  where
    th = chunksInsertSize config
    w  = Table.width tbl

-- | Make 'StringSQL' string of SQL INSERT record chunk statement from 'InsertTarget'
sqlChunkFromInsertTarget :: Config
                         -> Record PureOperand p
                         -> Table r
                         -> InsertTarget p r
                         -> (SQLWithPlaceholderOffsets', Int)
sqlChunkFromInsertTarget config phs tbl it =
    (sqlChunkFromInsertTarget' config n phs tbl it, n)
  where
    n = countChunks config tbl

-- | Make 'StringSQL' string of SQL INSERT statement from 'InsertTarget'
sqlFromInsertTarget :: Config -> Record PureOperand p -> Table r -> InsertTarget p r -> SQLWithPlaceholderOffsets'
sqlFromInsertTarget config = sqlChunkFromInsertTarget' config 1

-- | Make 'StringSQL' strings of SQL INSERT strings from records list
sqlChunksFromRecordList :: LiteralSQL r'
                        => Config
                        -> Table r
                        -> Pi r r'
                        -> [r']
                        -> [SQLWithPlaceholderOffsets']
sqlChunksFromRecordList config tbl pi' xs =
    [ (\cs -> INSERT <> INTO <> stringSQL (Table.name tbl) <> cs)
      <$>
        composeValuesListWithColumns
        [ tf tbl
        | r <- rs
        , let ((), tf) = Register.extract (pi' <-# Record.toFlat (value r)) config
        ]
    | rs <- unfoldr step xs
    ]
  where
    n = countChunks config tbl
    step ys
      | null ys    =  Nothing
      | otherwise  =  Just $ splitAt n ys
