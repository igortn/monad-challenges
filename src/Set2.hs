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
                                               Just m1 -> case headMay ns of
                                                            Nothing -> Nothing
                                                            Just h -> case divMay (fromIntegral m1) (fromIntegral h) of
                                                                        Nothing -> Nothing
                                                                        Just x -> Just x

-----------------------------------------------

