myLcm :: Integral a => a -> a -> a
myLcm x y = abs (x * y) `div` gcd x y

main = do
    _ <- getLine
    input <- getLine
    let list = map read $ words input :: [Integer]
    print $ foldr1 myLcm list