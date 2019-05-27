{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad (unless, forM_)
import Data.Barbie (bzip, bfoldMap)
import Data.Functor.Const (Const (Const))
import Data.Functor.Product (Product (Pair))
import Data.List (isPrefixOf)
import Database.Relational
import System.Environment (lookupEnv)
import System.IO (Handle, IOMode (WriteMode), withFile, hPrint, hPutStrLn)
import Test.QuickCheck (forAll)

import Test.Relational.QuickCheck.Placeholders.Gen
import Test.Relational.QuickCheck.Placeholders.Model

main :: IO ()
main = do
  logPath <- lookupEnv "HRR_TEST_LOG"
  withLog logPath (hspec . spec)

spec :: LogHandle -> Spec
spec lh = describe "placeholder feature of HRR" $ do
  prop "SimpleQuery" . forAll genRelationWithInputs $ \(acts, phs, rel) -> do
    let actualPhos   = extractPlaceholderOffsets $ composeSubQuery rel
        expectedPhos = toOffsets phs
    lPutStrLn lh "------------------------- SimpleQuery -------------------"
    debugLog lh acts rel actualPhos expectedPhos
    actualPhos `bshouldStartWithCycled` expectedPhos

  prop "AggregatedQuery" . forAll genAggregatedRelationWithInputs $ \(acts, phs, rel) -> do
    let actualPhos   = extractPlaceholderOffsets $ composeSubQuery rel
        expectedPhos = toOffsets phs
    lPutStrLn lh "------------------------- AggregatedQuery -------------------"
    debugLog lh acts rel actualPhos expectedPhos
    actualPhos `bshouldStartWithCycled` expectedPhos

----- Assertion

bshouldStartWithCycled
  :: forall a. (HasCallStack, Show a, Eq a) => InEveryClause [a] -> InEveryClause [a] -> Expectation
bshouldStartWithCycled a = bfoldMap f . bzip a
 where
  f :: Product (Const [a]) (Const [a]) b -> Expectation
  f (Pair (Const as) (Const es)) = as `shouldStartWithCycled` es

shouldStartWithCycled :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldStartWithCycled = compareWith f "does not start with cycled"
 where
  f es as = as `isPrefixOf` cycle es

-- Copied from http://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/src/Test-Hspec-Expectations.html#compareWith
compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
 where
  errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

-- Copied from http://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/src/Test-Hspec-Expectations.html#expectTrue
expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

----- Logging

type LogHandle = Maybe Handle

withLog :: Maybe FilePath -> (LogHandle -> IO ()) -> IO ()
withLog Nothing action = action Nothing
withLog (Just fp) action = withFile fp WriteMode (action . Just)

debugLog :: Show a => LogHandle -> [a] -> Relation Ph b -> InEveryClause [Int] -> InEveryClause [Int] -> IO ()
debugLog lh acts sq actualPhos expectedPhos = do
  lPutStrLn lh "/* Input Action:"
  mapM_ (lPrint lh) acts
  lPutStrLn lh "Input Action: */"
  lPrint lh sq
  lPutStrLn lh $ "ACTUAL: " ++ show actualPhos
  lPutStrLn lh $ "EXPECTED: " ++ show expectedPhos

lPrint :: Show a => LogHandle -> a -> IO ()
lPrint lh x = forM_ lh (`hPrint` x)

lPutStrLn :: LogHandle -> String -> IO ()
lPutStrLn lh x = forM_ lh (`hPutStrLn` x)
