fact :: Int -> Int
fact 1 = 1
fact n = n * fact(n-1)

main :: IO ()
main = do
    n <- readLn
    print $ fact n