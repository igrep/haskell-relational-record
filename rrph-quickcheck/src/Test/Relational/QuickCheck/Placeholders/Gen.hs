{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Relational.QuickCheck.Placeholders.Gen
  ( CommonAction (..)
  , SimpleAction (..)
  , AggregatedAction (..)
  , ReturnVal (..)
  , Op (..)
  , VarExpr (..)
  , Expr (..)
  , Cmp (..)
  , Term (..)
  , Pred (..)
  , InEveryClause

  , genRelationWithInputs
  , genAggregatedRelationWithInputs
  , extractPlaceholderOffsets
  , toOffsets
  , composeSubQuery
  ) where

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Data.Barbie
  (FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC, AllBF,
   bmap, bzipWith,
   Rec(..), {- Required to derive FunctorB, etc.-})
import Data.Bifunctor (first)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import Data.Generics.Product.Fields (field')
import Data.Int (Int64)
import Database.Record
import Database.Relational
import Database.Relational.Projectable.Unsafe (OperatorContext)
import Database.Relational.Monad.Trans.Aggregating (AggregatingPowerSet, AggregatingSetList, AggregatingSet)
import Database.Relational.SqlSyntax (placeholderOffsetsOfRecord, SubQuery (..))
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import Lens.Micro.Mtl (use, (.=))
import QuickCheck.GenT (GenT (unGenT), elements, liftGen)
import Test.QuickCheck (Arbitrary (..), getNonEmpty)
import Test.QuickCheck.Gen (Gen (MkGen))

import Test.Relational.QuickCheck.Placeholders.Model
import Test.Relational.QuickCheck.Placeholders.Types

type GenFromInputT m = GenT (StateT Input m)

data Input = Input
  { inputRelations :: ![RelationAB]
  , inputSubqueryRecordSelectors :: ![Selected Flat]
  , inputGroupedRecordSelectors :: ![Selected Aggregated]
  , inputPhSelectors :: PhSelectors
  } deriving Generic

appendSubqueryRecord :: RecordAB Flat -> Input -> Input
appendSubqueryRecord r i =
  i { inputSubqueryRecordSelectors = selectorsFromRecord r ++ inputSubqueryRecordSelectors i }

appendGroupedRecord :: Selected Aggregated -> Input -> Input
appendGroupedRecord r i =
  i { inputGroupedRecordSelectors = r : inputGroupedRecordSelectors i }

selectorsFromRecord :: RecordAB c -> [Selected c]
selectorsFromRecord (Left a)  = map (a !) [a0', a1', a2']
selectorsFromRecord (Right b) = map (b !) [b0', b1', b2']

type BaseSelected f = f Int64

type Selected c = BaseSelected (Record c)

data InEveryClauseB f = InEveryClauseB
  { inReturn :: !(f ())
  , inOn :: !(f ())
  , inWheres :: !(f ())
  , inGroupBy :: !(f ())
  , inHaving :: !(f ())
  , inOrderBy :: !(f ())
  } deriving Generic

instance FunctorB InEveryClauseB
instance TraversableB InEveryClauseB
instance ProductB InEveryClauseB
instance ConstraintsB InEveryClauseB
instance ProductBC InEveryClauseB

deriving instance AllBF Show f InEveryClauseB => Show (InEveryClauseB f)
deriving instance AllBF Eq f InEveryClauseB => Eq (InEveryClauseB f)

type InEveryClause a = InEveryClauseB (Const a)

instance Semigroup a => Semigroup (InEveryClause a) where
  (<>) = bzipWith (<>)

instance Monoid a => Monoid (InEveryClause a) where
  mempty = InEveryClauseB mempty mempty mempty mempty mempty mempty

type PhSelectors = InEveryClause [PhSelector]

instance Arbitrary PhSelectors where
  arbitrary = InEveryClauseB
    <$> (Const . getNonEmpty <$> arbitrary)
    <*> (Const . getNonEmpty <$> arbitrary)
    <*> (Const . getNonEmpty <$> arbitrary)
    <*> (Const . getNonEmpty <$> arbitrary)
    <*> (Const . getNonEmpty <$> arbitrary)
    <*> (Const . getNonEmpty <$> arbitrary)

type PhOffsets = InEveryClause [Int]

toOffsets :: PhSelectors -> PhOffsets
toOffsets = bmap $ first (F.foldMap f)
 where
  f sel = F.toList . placeholderOffsetsOfRecord . runSelectorPh sel $ toSomeOperatorContext defaultPlaceholders

extractPlaceholderOffsets :: SubQuery -> PhOffsets
extractPlaceholderOffsets (Table _)                                       = mempty
extractPlaceholderOffsets (Flat _cfg tup _dup jp pds ots)                = InEveryClauseB
  { inReturn = Const . F.toList $ placeholderOffsets tup
  , inOn = Const . F.toList $ placeholderOffsets jp
  , inWheres = Const . F.toList $ F.foldMap placeholderOffsets pds
  , inGroupBy = mempty
  , inHaving = mempty
  , inOrderBy =  Const . F.toList $ placeholderOffsets ots
  }
extractPlaceholderOffsets (Aggregated _cfg tup _dup jp pdfs aes pdas ots) = InEveryClauseB
  { inReturn = Const . F.toList $ placeholderOffsets tup
  , inOn = Const . F.toList $ placeholderOffsets jp
  , inWheres = Const . F.toList $ F.foldMap placeholderOffsets pdfs
  , inGroupBy = Const . F.toList $ placeholderOffsets aes
  , inHaving = Const . F.toList $ foldMap placeholderOffsets pdas
  , inOrderBy =  Const . F.toList $ placeholderOffsets ots
  }
extractPlaceholderOffsets (Bin _op sqx sqy)                               = extractPlaceholderOffsets sqx <> extractPlaceholderOffsets sqy

composeSubQuery :: PersistableWidth p => Relation p a -> SubQuery
composeSubQuery = (`configureQuery` defaultConfig) . (`untypeRelation` defaultPlaceholders)

cyclePhSelectors :: PhSelectors -> PhSelectors
cyclePhSelectors = bmap $ first cycle

gLift :: Monad m => m a -> GenFromInputT m a
gLift = lift . lift

newtype BasePhSelector f = BasePhSelector { runSelectorPh :: f Ph -> f Int64 } deriving Generic

type PhSelector = BasePhSelector (Record PureOperand)

instance Arbitrary PhSelector where
  arbitrary = elements $ map (BasePhSelector . flip (!)) [ph0', ph1', ph2']

instance Show PhSelector where
  show s = unwords ["PhSelector", show . runSelectorPh s $ toSomeOperatorContext defaultPlaceholders]

type ResultInts = (Int64, Int64, Int64, Int64, Int64, Int64, Int64)

type ReturnVals = (ReturnVal, ReturnVal, ReturnVal, ReturnVal, ReturnVal, ReturnVal, ReturnVal)

genRelationWithInputs :: Gen ([SimpleAction], PhSelectors, Relation Ph ResultInts)
genRelationWithInputs = do
  (acts, rvals, phs) <- arbitrary
  -- The first `AQuery` is necessary to initialize `inputSubqueryRecordSelectors`
  let completeActs = SimpleCommon AQuery : acts
  rel <- runSimpleAction completeActs rvals phs
  return (acts, phs, rel)

genAggregatedRelationWithInputs :: Gen ([AggregatedAction], PhSelectors, Relation Ph ResultInts)
genAggregatedRelationWithInputs = do
  (acts, rvals, phs) <- arbitrary
  -- The first `AQuery` and (`AGroupBy` or `AGroupBy') are necessary to initialize `inputSubqueryRecordSelectors`
  grp <- join $ elements [AGroupBy <$> arbitrary, AGroupBy' <$> arbitrary]
  let completeActs = AggregatedCommon AQuery : grp : acts
  rel <- runAggregatedAction completeActs rvals phs
  return (acts, phs, rel)

runSomeAction
  :: Monad m
  => ((Record PureOperand Ph -> m (Record c ResultInts)) -> (Relation Ph ResultInts))
  -> (Record PureOperand Ph -> GenFromInputT m (Record c ResultInts))
  -> InEveryClauseB (Const [PhSelector])
  -> Gen (Relation Ph ResultInts)
runSomeAction mkRel eval phs =
  MkGen $ \qcgen sz ->
    mkRel $ \ph ->
      (`evalStateT` initialInput) $ unGenT (eval ph) qcgen sz
 where
  initialInput = Input
    { inputRelations = [Left relA, Right relB]
    , inputSubqueryRecordSelectors = []
    , inputPhSelectors = cyclePhSelectors phs
    , inputGroupedRecordSelectors = []
    }

runSimpleAction :: [SimpleAction] -> ReturnVals -> PhSelectors -> Gen (Relation Ph ResultInts)
runSimpleAction acts rvals = runSomeAction relation' $ evalSimpleAction acts rvals

runAggregatedAction :: [AggregatedAction] -> ReturnVals -> PhSelectors -> Gen (Relation Ph ResultInts)
runAggregatedAction acts rvals = runSomeAction aggregateRelation' $ evalAggregatedAction acts rvals

evalSimpleAction :: [SimpleAction] -> ReturnVals -> Record PureOperand Ph -> GenFromInputT QuerySimple (Record Flat ResultInts)
evalSimpleAction acts rvals ph =
  F.traverse_ interpret acts >> aReturn inputSubqueryRecordSelectors ph rvals
 where
  interpret (SimpleCommon ca) = interpretCommon ph ca
  interpret (AFlatOrderBy ex) = aOrderBy chooseColumn ph ex

evalAggregatedAction :: [AggregatedAction] -> ReturnVals -> Record PureOperand Ph -> GenFromInputT QueryAggregate (Record Aggregated ResultInts)
evalAggregatedAction acts rvals ph =
  -- The first `aQuery` and `aGroupBy` are necessary to initialize `inputSubqueryRecordSelectors`
  F.traverse_ interpret acts >> aReturn inputGroupedRecordSelectors ph rvals
 where
  interpret (AggregatedCommon ca) = interpretCommon ph ca
  interpret (AGroupBy ex) = aGroupBy ph ex
  interpret (AGroupBy' ex) = aGroupBy' ph ex
  interpret (AHaving pd) = aRestrict chooseGroupedColumn (field' @"inHaving") ph pd
  interpret (AAggregatedOrderBy ex) = aOrderBy chooseGroupedColumn ph ex

interpretCommon :: (MonadQuery m, MonadRestrict Flat m) => Record PureOperand Ph -> CommonAction -> GenFromInputT m ()
interpretCommon _ph AQuery = aQuery
interpretCommon ph (AOn pd) =
  gLift . on =<< predSQL chooseColumn (popPlaceholder ph $ field' @"inOn") pd
interpretCommon ph (AWheres pd) =
  aRestrict chooseColumn (field' @"inWheres") ph pd

aQuery :: MonadQuery m => GenFromInputT m ()
aQuery = do
  -- TODO: add subquery as relation
  ab <- elements =<< lift (gets inputRelations)
  rd <- case ab of
    Left rel ->  fmap Left . gLift $ query rel
    Right rel -> fmap Right . gLift $ query rel
  lift . modify' $ appendSubqueryRecord rd

aGroupBy :: MonadAggregate m => Record PureOperand Ph -> Expr Flat -> GenFromInputT m ()
aGroupBy ph expr = do
  grd <- gLift . groupBy =<< exprSQL chooseColumn (popPlaceholder ph $ field' @"inGroupBy") expr
  lift . modify' $ appendGroupedRecord grd

aGroupBy' :: MonadAggregate m => Record PureOperand Ph -> AgExpr -> GenFromInputT m ()
aGroupBy' ph expr = do
  grd <- gLift . groupBy' =<< agExprSQL chooseColumn (popPlaceholder ph $ field' @"inGroupBy") expr
  lift . modify' $ appendGroupedRecord grd

aReturn
  :: forall m c
   . (Monad m, OperatorContext c)
  => (Input -> [Selected c])
  -> Record PureOperand Ph
  -> ReturnVals
  -> GenFromInputT m (Record c ResultInts)
aReturn getSel ph (rv1, rv2, rv3, rv4, rv5, rv6, rv7) = do
  i1 <- chooseReturnedSelector rv1
  i2 <- chooseReturnedSelector rv2
  i3 <- chooseReturnedSelector rv3
  i4 <- chooseReturnedSelector rv4
  i5 <- chooseReturnedSelector rv5
  i6 <- chooseReturnedSelector rv6
  i7 <- chooseReturnedSelector rv7
  return $ (,,,,,,) |$| i1 |*| i2 |*| i3 |*| i4 |*| i5 |*| i6 |*| i7
 where
  chooseReturnedSelector :: ReturnVal -> GenFromInputT m (Record c Int64)
  chooseReturnedSelector ReturnPh =
    toSomeOperatorContext <$> popPlaceholder ph (field' @"inReturn")
  chooseReturnedSelector ReturnSelected =
    elements =<< lift (gets getSel)

aOrderBy
  :: (Monad m, OperatorContext c)
  => GenFromInputT (Orderings c m) (Selected c)
  -> Record PureOperand Ph
  -> Expr c
  -> GenFromInputT (Orderings c m) ()
aOrderBy chooseCol ph expr =
  gLift . asc =<< exprSQL chooseCol (popPlaceholder ph $ field' @"inOrderBy") expr

aRestrict
  :: (MonadRestrict c m, OperatorContext c)
  => GenFromInputT m (Selected c)
  -> Lens' PhSelectors (Const [PhSelector] ())
  -> Record PureOperand Ph
  -> Pred c
  -> GenFromInputT m ()
aRestrict chooseCol lensPh ph expr =
  gLift . restrict =<< predSQL chooseCol (popPlaceholder ph lensPh) expr

popPlaceholder :: Monad m => Record PureOperand Ph -> Lens' PhSelectors (Const [PhSelector] ()) -> GenFromInputT m (Record PureOperand Int64)
popPlaceholder phRec lensPh = lift $ do
  i <- getConst <$> use (field' @"inputPhSelectors" . lensPh)
  case i of
      [] -> error "popPlaceholder: Empty placeholder selectors"
      (phsel : leftSels) -> do
        (field' @"inputPhSelectors" . lensPh) .= Const leftSels
        return $ runSelectorPh phsel phRec

chooseColumn :: Monad m => GenFromInputT m (Selected Flat)
chooseColumn = elements =<< lift (gets inputSubqueryRecordSelectors)

chooseGroupedColumn :: Monad m => GenFromInputT m (Record Aggregated Int64)
chooseGroupedColumn = elements =<< lift (gets inputGroupedRecordSelectors)

opSQL :: OperatorContext c
      => Op
      -> Record c Int64
      -> Record c Int64
      -> Record c Int64
opSQL = d  where
  d Plus   = (.+.)
  d Minus  = (.-.)

varExprSQL :: (Monad m, OperatorContext c)
           => GenFromInputT m (Selected c)
           -> GenFromInputT m (Record PureOperand Int64)
           -> VarExpr c
           -> GenFromInputT m (Record c Int64)
varExprSQL chooseCol popPh = d where
  d Column = chooseCol
  d Placeholder = toSomeOperatorContext <$> popPh
  d (VLeft  ve op i) = opSQL op <$> d ve <*> pure (toSomeOperatorContext (value i))
  d (VRight i op ve) = opSQL op (toSomeOperatorContext (value i)) <$> d ve

exprSQL :: (Monad m, OperatorContext c)
        => GenFromInputT m (Selected c)
        -> GenFromInputT m (Record PureOperand Int64)
        -> Expr c
        -> GenFromInputT m (Record c Int64)
exprSQL chooseCol popPh = d where
  d (Var ve)     = varExprSQL chooseCol popPh ve
  d (e0 :+: e1)  = (.+.) <$> d e0 <*> d e1
  d (e0 :-: e1)  = (.-.) <$> d e0 <*> d e1

cmpSQL :: OperatorContext c
       => Cmp
       -> Record c a
       -> Record c a
       -> Record c (Maybe Bool)
cmpSQL = d  where
  d Lt = (.<.)
  d Eq = (.=.)
  d Gt = (.>.)

termSQL :: (Monad m, OperatorContext c)
        => GenFromInputT m (Selected c)
        -> GenFromInputT m (Record PureOperand Int64)
        -> Term c
        -> GenFromInputT m (Record c (Maybe Bool))
termSQL chooseCol popPh (Term (e0, op, e1)) = cmpSQL op <$> (exprSQL chooseCol popPh e0) <*> (exprSQL chooseCol popPh e1)

predSQL :: (Monad m, OperatorContext c)
        => GenFromInputT m (Selected c)
        -> GenFromInputT m (Record PureOperand Int64)
        -> Pred c
        -> GenFromInputT m (Record c (Maybe Bool))
predSQL chooseCol popPh = d  where
  d (PTerm t)    = termSQL chooseCol popPh t
  d (Not p)      = not' <$> d p
  d (p0 :&: p1)  = and' <$> d p0 <*> d p1
  d (p0 :|: p1)  = or' <$> d p0 <*> d p1

agExprSQL :: Monad m
          => GenFromInputT m (Selected Flat)
          -> GenFromInputT m (Record PureOperand Int64)
          -> AgExpr
          -> GenFromInputT m (AggregateKey (Record Aggregated Int64))
agExprSQL chooseCol popPh (Rollup apse) = do
  d <- genDefaultValue
  rollup <$> getCompose (fromMaybe d <$> Compose (aggregatingPowerSetExprSQL chooseCol popPh apse))
agExprSQL chooseCol popPh (Cube apse) = do
  d <- genDefaultValue
  cube <$> getCompose (fromMaybe d <$> Compose (aggregatingPowerSetExprSQL chooseCol popPh apse))
agExprSQL chooseCol popPh (GroupingSets asle) =
  groupingSets <$> aggregatingSetListExprSQL chooseCol popPh asle

aggregatingPowerSetExprSQL :: Monad m
                           => GenFromInputT m (Selected Flat)
                           -> GenFromInputT m (Record PureOperand Int64)
                           -> AggregatingPowerSetExpr
                           -> GenFromInputT m (AggregatingPowerSet (Record Aggregated (Maybe Int64)))
aggregatingPowerSetExprSQL chooseCol popPh (e1, e2) = getCompose $
  (?+?) <$> Compose (bkey <$> exprSQL chooseCol popPh e1) <*> Compose (bkey <$> exprSQL chooseCol popPh e2)

aggregatingSetListExprSQL :: Monad m
                          => GenFromInputT m (Selected Flat)
                          -> GenFromInputT m (Record PureOperand Int64)
                          -> AggregatingSetListExpr
                          -> GenFromInputT m (AggregatingSetList (Record Aggregated Int64))
aggregatingSetListExprSQL chooseCol popPh (e1, e2) = getCompose $
  (.+.) <$> Compose (set <$> aggregatingSetExprSQL chooseCol popPh e1) <*> Compose (set <$> aggregatingSetExprSQL chooseCol popPh e2)

aggregatingSetExprSQL :: Monad m
                      => GenFromInputT m (Selected Flat)
                      -> GenFromInputT m (Record PureOperand Int64)
                      -> AggregatingSetExpr
                      -> GenFromInputT m (AggregatingSet (Record Aggregated Int64))
aggregatingSetExprSQL chooseCol popPh (AgKey e) = do
  grp <- return . key =<< exprSQL chooseCol popPh e
  d <- genDefaultValue
  return (fromMaybe d <$> grp)
aggregatingSetExprSQL chooseCol popPh (AgKey' e) = do
  return . key' =<< agExprSQL chooseCol popPh e

genDefaultValue :: (OperatorContext c, Monad m) => GenFromInputT m (Record c Int64)
genDefaultValue = toSomeOperatorContext . value <$> liftGen arbitrary
