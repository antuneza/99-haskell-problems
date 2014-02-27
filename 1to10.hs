-- Problems 1 to 10 of 99 Haskell Problems
-- Lists
-- Fernando Antunez Garcia

myLast :: [a] -> a
myLast xs = head $ reverse xs

myButLast :: [a] -> a
myButLast xs = head . tail $ reverse xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (++) [] $ map flatten xs

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = foldr (\x acc -> if (x == head acc) then acc else x:acc) [last xs] xs 

pack :: (Eq a) => [a] -> [[a]]
pack = foldr pack' []
  where
    pack' x [] = [[x]]
    pack' x (a:acc) = if (x == head a) then (x:a):acc else [x]:a:acc

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

