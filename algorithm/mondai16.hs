solve :: [Int] -> Int
solve = foldr1 gcd 

main = do
    _ <- getLine
    input <- getLine
    let list = map read $ words input :: [Int]
    print $ solve list