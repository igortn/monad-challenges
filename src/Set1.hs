{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [n1, n2, n3, n4, n5]
  where (n1, s1) = rand (mkSeed 1)
        (n2, s2) = rand s1
        (n3, s3) = rand s2
        (n4, s4) = rand s3
        (n5, s5) = rand s4

randLetter :: Gen Char
randLetter s = (toLetter n, s')
  where (n, s') = rand s

randString3 :: String
randString3 = [c1, c2, c3]
  where (c1, s1) = randLetter (mkSeed 1)
        (c2, s2) = randLetter s1
        (c3, s3) = randLetter s2

randEven :: Gen Integer
randEven s = (n*2, s')
  where (n, s') = rand s

randOdd :: Gen Integer
randOdd s = (n+1, s')
  where (n, s') = randEven s

randTen :: Gen Integer
randTen s = (10*n, s')
  where (n, s') = rand s


-- Generalize the combination of rand and --
-- a function application --

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen s = (f x, s')
  where (x, s') = gen s

randLetter' :: Gen Char
randLetter' = generalA toLetter rand

randEven' :: Gen Integer
randEven' = generalA (2*) rand

randOdd' :: Gen Integer
randOdd' = generalA (\n -> 2*n+1) rand

--------------------------------------------

randPair :: Gen (Char, Integer)
randPair s = ((c, n), s'')
  where (c, s') = randLetter s
        (n, s'') = rand s'

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g1 g2 s = ((t1,t2), s2)
  where (t1,s1) = g1 s
        (t2,s2) = g2 s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 s = (f t1 t2, s2)
  where (t1,s1) = g1 s
        (t2,s2) = g2 s1

generalPair' :: Gen a -> Gen b -> Gen (a,b)
generalPair' = generalB (\a b -> (a,b))

---------------------------------------------

repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (g:gs) s = (x:xs, s'')
  where (x,s') = g s
        (xs,s'') = repRandom gs s'

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = f x s'
  where (x,s') = g s

mkGen :: a -> Gen a
mkGen x s = (x,s)

