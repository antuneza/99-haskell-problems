-- Problems 11 to 20 of 99 Haskell Problems
-- Lists, continued
-- Fernando Antunez Garcia

-- Includes problems 9 and 10 from previous file

pack :: (Eq a) => [a] -> [[a]]
pack = foldr pack' []
  where
    pack' x [] = [[x]]
    pack' x (a:acc) = if (x == head a) then (x:a):acc else [x]:a:acc

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data EncodingItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodingItem a] 
encodeModified = map(\(n,x) -> if n == 1 then Single x else Multiple n x) . encode

decodeModified :: [EncodingItem a] -> [a]
decodeModified = concatMap f
  where   f (Single x) = [x]
    f (Multiple n x) = replicate n x 

encodeDirect :: (Eq a) => [a] -> [EncodingItem a]
encodeDirect = map (\(n,x) -> if n == 1 then Single x else Multiple n x) . encode'

encode' :: (Eq a) => [a] -> [(Int,a)]
encode' = reverse . foldl f []
  where
    f [] x = [(1,x)]
    f ((n,a):acc) x = if (x == a) then ((n+1,a):acc) else (1,x):(n,a):acc       

dupli :: [a] -> [a]
dupli = concatMap (\x -> x:[x])

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
-- Amazing solution from someone else
-- dropEvery = flip $ \n -> map snd . filter ((n/=) . fst) . zip (cycle [1..n])
dropEvery xs n = f xs n 
  where 
    f [] _ = []
    f (x:xs) 1 = f xs n
    f (x:xs) k = x:(f xs (k-1))

split :: [a] -> Int -> ([a],[a])
-- I can't use predefined predicates :(
-- split =  flip $ \n -> foldr (\(k,x) (a,b)-> if (k==0) then (x:a,b) else (a,x:b)) ([],[]) . zip ((replicate n 0) ++ repeat 1) 

-- Using ++  not efficiently. I can be improved
split xs n = f xs n ([],[])
  where
    f xs 0 (a,_) = (a,xs)
     f (x:xs) n (a,_) = f xs (n-1) (a++[x],[])

slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs

rotate :: [a] -> Int -> [a]
rotate xs n 
  | n < 0 = reverse $ rotate (reverse xs) (-n)
  | n > 0 = drop n xs ++ take n xs
  | otherwise = []

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), (take (n-1) xs) ++ (drop n xs))

