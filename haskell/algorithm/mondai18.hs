import qualified Data.Set as Set

countPairs :: [Int] -> Int
countPairs prices = length [(x, y) | x <- prices, y <- prices, x + y == 500] `div` 2

main = do
    _ <- getLine
    input <- getLine
    let prices = map read $ words input :: [Int]
    print $ countPairs prices

