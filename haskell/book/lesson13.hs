data Icecream = Chocolate | Vanilla deriving(Show, Ord, Eq)

inc :: Int -> Int
inc x = x + 1

cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc a = succ a