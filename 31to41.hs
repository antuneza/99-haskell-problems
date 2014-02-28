-- Problems 31 to 41 of 99 Haskell Problems
-- Arithmetics
-- Fernando Antunez Garcia

import Data.List

isPrime :: Int -> Bool
isPrime n = dropWhile (\x -> n `mod` x /=0) [2..round(sqrt ( fromIntegral n))] == []

myGCD :: Int -> Int -> Int
myGCD a b 
  | r == 0 = b
  | otherwise = myGCD b r
  where r = a `mod` b

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

totient :: Int -> Int
totient 1 = 1
totient m = length $ filter (coprime m) [1..m-1]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = sort $ let prims = [a | a <- primesR 2 n, n `mod` a == 0] in prims ++ primeFactors (n `div` product prims)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\xs -> (head xs, length xs)) . group . primeFactors

totientImp :: Int -> Int
totientImp = product . map (\(p,m) -> (p-1) * (p ^ (m-1)) ) . primeFactorsMult

-- How to compare totient and totientImp ???

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

goldbach :: Int -> (Int, Int)
goldbach n = head [(a,b) | a <- primesR 2 n, let b = n - a, isPrime b]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = [goldbach x | x<-[a..b], even x]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b t = filter (\(i,j) -> i>t && j>t ) $ goldbachList a b
