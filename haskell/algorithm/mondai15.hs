myGcd :: Int -> Int -> Int
myGcd x 0 = x
myGcd x y = myGcd y (x `mod` y)

main = do
    input <- getLine
    let [x, y] = map read $ words input :: [Int]
    print $ myGcd x y