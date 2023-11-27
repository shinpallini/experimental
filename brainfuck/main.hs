import Data.Array

initArray i = array (1, i) [(i, 0) | i <- [1..i]]

main = do
    print $ initArray 10000 ! 9987