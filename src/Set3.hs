{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] ->[(a,b)]
allPairs [] _ = []
allPairs (x:xs) ys = map (\z -> (x,z)) ys ++ allPairs xs ys

data Card = Card Int String

instance Show Card where
  show (Card n s) = show n ++ s

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (n:ns) ss = map (Card n) ss ++ allCards ns ss

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs = allCombs (f x) ys zs ++ allCombs3 f xs ys zs

-- test with allCombs3 (,,) [1,2] [3,4] [5,6]

---- generalization of allCombs ----------------

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) xs = map f xs ++ combStep fs xs

allCombs2' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs2' f xs = combStep (combStep [f] xs)

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys = combStep (combStep (combStep [f] xs) ys)