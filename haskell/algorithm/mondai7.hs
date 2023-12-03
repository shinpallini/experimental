import Text.XHtml (input)
solve :: Int -> Int -> Int -> Int
solve n x y = n `div` x + n `div` y - n `div` (lcm x y)

main :: IO ()
main = do
    input <- getLine
    let [n, x, y]= map read (words input)
    print $ solve n x y