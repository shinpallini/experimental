import Data.List (sort)

check :: Int -> Int -> Bool
check n x = n `mod` x == 0

solve :: Int -> [Int] -> [Int]
solve _ [] = []
solve n (x:xs) = if check n x
    then x:(n `div` x):solve n xs
    else solve n xs

output :: [Int] -> String
output l = unlines $  map show (sort l)
main = do
    n <- readLn
    putStrLn $ output $ solve n [1..floor . sqrt $ fromIntegral n]