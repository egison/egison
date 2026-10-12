{-# LANGUAGE TemplateHaskell #-}

-- | Checks of the Sweet Egison side of the comparison with Haskell
-- embeddings in the related work of the paper on type inference for Egison
-- pattern matching.
module Main (main) where

import           Control.Exception (TypeError (..), evaluate, try)
import           Data.List         (isInfixOf)
import           Data.Maybe        (catMaybes)
import           Test.HUnit

import           Control.Egison
import           Definitions       (unconsWith)
import           InferredType      (unconsWithType)
import qualified Rejected

main :: IO ()
main = runTestTTAndExit $ TestList
  [ "unconsWith is inferred with Matcher and CollectionPattern constraints" ~: do
      let (printed, difference) = $(unconsWithType)
      putStrLn ("unconsWith :: " ++ printed)
      maybe (return ()) assertFailure difference
  , "unconsWith with List Something" ~:
      unconsWith (List Something) ([1, 2, 3] :: [Int]) @?= [(1, [2, 3])]
  , "unconsWith with Multiset Something" ~:
      unconsWith (Multiset Something) ([1, 2, 3] :: [Int])
        @?= [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
  , "the cons pattern under Something is rejected" ~: do
      message <- typeError (length Rejected.unsupportedCons)
      putStrLn ("unsupportedCons: " ++ show message)
      assertMentions message ["No instance for", "CollectionPattern Something [Int]"]
  , "chooseMatcher is rejected" ~: do
      messages <- catMaybes <$> mapM (typeError . Rejected.chooseMatcher) [True, False]
      putStrLn ("chooseMatcher: " ++ show messages)
      assertBool "no type error for chooseMatcher" (not (null messages))
      mapM_ (\message -> assertMentions (Just message)
               ["Couldn't match", "List Something", "Multiset Something"]) messages
  ]

-- | The deferred type error, if any, raised by evaluating a value.
typeError :: a -> IO (Maybe String)
typeError expression = do
  result <- try (evaluate expression)
  return $ case result of
    Left (TypeError message) -> Just message
    Right _                  -> Nothing

assertMentions :: Maybe String -> [String] -> Assertion
assertMentions Nothing _ = assertFailure "no type error"
assertMentions (Just message) phrases =
  mapM_ (\phrase -> assertBool ("the type error does not mention " ++ show phrase)
                      (phrase `isInfixOf` message)) phrases
