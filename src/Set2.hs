{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

-----------------------------------------

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay k ((k',v) : m') = if k==k' then Just v else lookupMay k m'

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x/y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = Just (max x y)
  where Just y = maximumMay xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:xs) = Just (min x y)
  where Just y = minimumMay xs

---------------------------------------------

queryGreek :: GreekData -> String -> Maybe Double
queryGreek dt s = case lookupMay s dt of
                    Nothing -> Nothing
                    Just ns -> case tailMay ns of
                                 Nothing -> Nothing
                                 Just nss -> case maximumMay nss of
                                               Nothing -> Nothing
                                               Just m -> case headMay ns of
                                                            Nothing -> Nothing
                                                            Just h -> case divMay (fromIntegral m) (fromIntegral h) of
                                                                        Nothing -> Nothing
                                                                        Just x -> Just x

-----------------------------------------------

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just x) f = f x

--queryGreek2 :: GreekData -> String -> Maybe Double
--queryGreek2 dt s = []
--  where m = link (link (lookupMay s dt) tailMay) maximumMay
--        h = link (lookupMay s dt) headMay

-----------------------------------------------

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries dt name1 name2 = case lookupMay name1 dt of
                               Nothing -> Nothing
                               Just sal1 -> case lookupMay name2 dt of
                                              Nothing -> Nothing
                                              Just sal2 -> Just (sal1 + sal2)

mkMaybe :: a -> Maybe a
mkMaybe = Just

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink _ Nothing _ = Nothing
yLink _ _ Nothing = Nothing
yLink f (Just t1) (Just t2) = Just (f t1 t2)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 dt name1 name2 = yLink (+) (lookupMay name1 dt) (lookupMay name2 dt)

---------------------------------------------

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd (x:xs) = Just (product xs)

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum (x:xs) = Just (sum xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing = Nothing
transMaybe f (Just x) = Just (f x)

tailProd' :: Num a => [a] -> Maybe a
tailProd' xs = transMaybe product (tailMay xs)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = case tailMay xs of
               Nothing -> Nothing
               Just ys -> maximumMay ys

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = case tailMay xs of
               Nothing -> Nothing
               Just ys -> minimumMay ys

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just Nothing) = Nothing
combine (Just (Just x)) = Just x
