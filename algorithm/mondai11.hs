solve :: Int -> [Int]
solve n = filter isPrime [2..n]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (isNotDivisible n) [2..end]
    where 
        end = floor $ sqrt (fromIntegral n)

isNotDivisible :: Int -> Int -> Bool
isNotDivisible x y = x `mod` y /= 0

main :: IO ()
main = do
    n <- readLn
    putStrLn $ unwords $ map show (solve n)
