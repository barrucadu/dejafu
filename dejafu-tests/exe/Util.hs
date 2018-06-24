module Util (ingredients, tests) where

import qualified Test.Tasty         as T
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Runners as T

import qualified Examples           as E
import qualified Integration        as I
import qualified Unit               as U

ingredients :: [T.Ingredient]
ingredients = T.includingOptions options : T.defaultIngredients

tests :: T.TestTree
tests = T.testGroup "Tests"
  [ T.testGroup "Unit" U.tests
  , T.testGroup "Integration" I.tests
  , T.testGroup "Examples" E.tests
  ]

options :: [T.OptionDescription]
options = U.options ++ I.options ++ E.options
