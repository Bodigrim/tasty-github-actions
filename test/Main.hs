module Main (main) where

import Test.Tasty
import Test.Tasty.GitHubActions
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMainWithIngredients (gitHubActionsIngredient : defaultIngredients) $
    testGroup
      "All"
      [ testGroup
          "Addition"
          [ testGroup
              "2"
              [ testCase "2 + 2 = 4" $ assertEqual "should be equal" (2 + 2) 4
              , testCase "2 + 3 = 5" $ assertEqual "should be equal" (2 + 3) 5
              , testCase "2 + 4 = 7" $ assertEqual "should be equal" (2 + 4) 7
              ]
          , testGroup
              "3"
              [ testCase "3 + 2 = 5" $ assertEqual "should be equal" (3 + 2) 4
              , testCase "3 + 3 = 6" $ assertEqual "should be equal" (3 + 3) 6
              , testCase "3 + 4 = 7" $ assertEqual "should be equal" (3 + 4) 7
              ]
          ]
      , testGroup
          "Multiplication"
          [ testGroup
              "2"
              [ testCase "2 * 2 = 3" $ assertEqual "should be equal" (2 * 2) 3
              , testCase "2 * 3 = 6" $ assertEqual "should be equal" (2 * 3) 6
              , testCase "2 * 4 = 8" $ assertEqual "should be equal" (2 * 4) 8
              ]
          , testGroup
              "3"
              [ testCase "3 * 2 = 6" $ assertEqual "should be equal" (3 * 2) 6
              , testCase "3 * 3 = 9" $ assertEqual "should be equal" (3 * 3) 9
              , testCase "3 * 4 = 8" $ assertEqual "should be equal" (3 * 4) 8
              ]
          ]
      ]
