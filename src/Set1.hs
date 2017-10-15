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

