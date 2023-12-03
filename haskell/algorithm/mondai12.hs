isPrime :: Int -> [Int] -> String
isPrime _ [] = "Yes"
isPrime n (x:xs) = if isDivisible n x
    then "No"
    else isPrime n xs

sqrtInt :: Int -> Int
sqrtInt n = floor $ sqrt (fromIntegral n)

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y == 0

main :: IO ()
main = do
    n <- readLn
    putStrLn $ isPrime n [2..sqrtInt n]