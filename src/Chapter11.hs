{-# LANGUAGE InstanceSigs #-}

module Chapter11 (main1, main2, computeThenPrint) where

import Data.Char (toUpper)
import Data.List (intersperse)

main1 :: IO ()
main1 = do
  line <- fmap reverse getLine
  putStrLn $ "you said " ++ line ++ " backwards"

main2 :: IO ()
main2 = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

computeThenPrint :: Integer -> String
computeThenPrint = fmap (show . (* 3)) (+ 100)

-- |Let's take a look at a pathological example of a type constructor 
-- being an instance of the Functor typeclass but not really being a functor,
-- because it doesn't satisfy the laws.
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap :: (a -> b) -> CMaybe a -> CMaybe b
  fmap _ CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)
