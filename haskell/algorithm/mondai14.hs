getFactor :: Int -> [Int] -> [Int]
getFactor 1 [] = []
getFactor n [] = [n]
getFactor n (x:xs) = if n `mod` x == 0
    then x:getFactor (n `div` x) (x:xs)
    else getFactor n xs

solve :: Int -> [Int]
solve n = getFactor n [2..floor . sqrt $ fromIntegral n]

main :: IO ()
main = do
    n <- readLn
    putStrLn $ unwords $ map show $ solve n

-- 素因数分解関数(ChatGPT)
-- primeFactors :: Int -> [Int]
-- primeFactors n = factorize n 2
--   where
--     factorize n f
--       | f * f > n      = [n | n > 1]
--       | n `mod` f == 0 = f : factorize (n `div` f) f
--       | otherwise      = factorize n (f + 1)

-- -- 主関数
-- main :: IO ()
-- main = do
--   putStrLn "Enter a number to factorize:"
--   input <- getLine
--   let number = read input :: Int
--   let factors = primeFactors number
--   putStrLn $ "Prime factors: " ++ unwords (map show factors)
