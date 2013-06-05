{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.Derives (
  unique,

  primary', primary,

  updateByConstraintKey,
  primaryUpdate
  ) where

import Database.Record (PersistableWidth)
import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Projectable (placeholder, (.=.))
import Database.Relational.Query.ProjectableExtended ((!))
import Database.Relational.Query.Monad.Class (wheres)
import Database.Relational.Query.Relation (Relation, relation', query)
import Database.Relational.Query.Constraint
   (Key, Primary, Unique, projectionKey, uniqueKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Query.Constraint as Constraint
import Database.Relational.Query.Type (Update, typedSingleKeyUpdate)

unique :: PersistableWidth p
       => Key Unique a p
       -> Relation () a
       -> Relation p a
unique uk rel = relation' $ do
  q <- query rel
  let (param, ph) = placeholder
  wheres $ q ! projectionKey uk .=. ph
  return (param, q)

primary' :: PersistableWidth p
         => Key Primary a p
         -> Relation () a
         -> Relation p a
primary' pc = unique $ Constraint.uniqueKey pc

primary :: (PersistableWidth p, HasConstraintKey Primary a p)
        => Relation () a
        -> Relation p a
primary = primary' constraintKey


updateByConstraintKey :: PersistableWidth p => Table r -> Key c r p -> Update p r
updateByConstraintKey table key =
  typedSingleKeyUpdate table (table `Table.index` Constraint.index key)

primaryUpdate :: (PersistableWidth p, HasConstraintKey Primary r p)
              => Table r -> Update p r
primaryUpdate table = updateByConstraintKey table (uniqueKey constraintKey)
