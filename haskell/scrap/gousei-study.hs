f :: Int -> Bool
f x = x > 3

g :: Int -> Bool
g x = x < 10

gousei :: Int -> [Int -> Bool] -> Bool
gousei x = all ($ x)
