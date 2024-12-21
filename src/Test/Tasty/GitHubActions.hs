{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}

{-# LANGUAGE ImplicitParams #-}

module Test.Tasty.GitHubActions (gitHubActionsIngredient) where

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Ingredients (Ingredient (..))
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter, buildTestOutput, MinDurationToReport, foldTestOutput, computeStatistics, printStatistics, statFailures)
import Test.Tasty.Runners (StatusMap, Time, TestTree, Traversal (..))
import Control.Monad (guard, when)
import Test.Tasty.Options (OptionSet, OptionDescription (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Any(..))

detectGHA :: Bool
detectGHA = unsafePerformIO $ do
  gha <- lookupEnv "GITHUB_ACTIONS"
  pure $ gha == Just "true"
{-# NOINLINE detectGHA #-}

gitHubActionsIngredient :: Ingredient
gitHubActionsIngredient =
  TestReporter
    [Option (Proxy :: Proxy MinDurationToReport)]
    (\opts tree -> if detectGHA then Just (ghaCallback opts tree) else Nothing)
  where
    ghaCallback :: OptionSet -> TestTree -> StatusMap -> IO (Time -> IO Bool)
    ghaCallback opts tree smap = do
      let ?colors = False
      let toutput = buildTestOutput opts tree
      getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
      pure $ \time -> do
        stats <- computeStatistics smap
        printStatistics stats time
        pure $ statFailures stats == 0
      where
        foldTest _name printName getResult printResult =
          ( Traversal $ do
              printName :: IO ()
              r <- getResult
              printResult r
          , Any True)
        foldHeading _name printHeading (printBody, Any nonempty) =
          ( Traversal $ do
              when nonempty $ do
                putStr "::group::"
                printHeading :: IO ()
                getTraversal printBody
                putStrLn "::endgroup::"
          , Any nonempty
          )

