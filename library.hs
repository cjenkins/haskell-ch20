module FoldableLibrary where

sum :: (Foldable t, Num a) => t a -> a
sum ta = foldr (+) 0 ta

product :: (Foldable t, Num a) => t a -> a
product ta = foldr (*) 1 ta

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a ta = foldr (\aInTa b -> (a == aInTa) || b) False ta

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta = foldr go Nothing ta where
  go a Nothing = Just a
  go a1 (Just a2) = if (a1 < a2) then Just a1 else Just a2

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta = foldr go Nothing ta where
  go a Nothing = Just a
  go a1 (Just a2) = if (a1 > a2) then Just a1 else Just a2

null :: (Foldable t) => t a -> Bool
null ta = foldr go True ta where
  go a _ = False

length :: (Foldable t) => t a -> Int
length ta = foldr go 0 ta where
  go a currentLength = currentLength + 1

toList :: (Foldable t) => t a -> [a]
toList ta = foldr go [] ta where
  go a currentList = a : currentList

fold :: (Foldable t, Monoid m) => t m -> m
fold tm = foldMap go tm where
  go a = mappend a mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr go mempty ta where
  go a b = mappend (f a) b
