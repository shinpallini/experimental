findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p = foldr (\x r -> if p x then Just x else r) Nothing
