import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

countOccurrences :: (Ord k, Num a) => [k] -> Map.Map k a
countOccurrences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

lookup' :: (Ord k, Num a) => Map.Map k a -> k -> a
lookup' prices key = Maybe.fromMaybe 0 (Map.lookup key prices)

main :: IO ()
main = do
    _ <- getLine
    input <- getLine
    let prices = map read $ words input :: [Int]
    -- let priceLookup = lookup' (countOccurrences prices)
    -- print $ priceLookup 100 * priceLookup 400 + priceLookup 200 * priceLookup 300
    let pricesMap = countOccurrences prices
    print $ pricesMap Map.! 100 * pricesMap Map.! 400 + pricesMap Map.! 200 * pricesMap Map.! 300