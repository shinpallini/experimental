type MyInt = Int

instance Semigroup MyInt where
  (<>) a b = a + b

instance Monoid MyInt where
  mempty = 0

newtype Events = Events [String]

instance Semigroup Events where
  (<>) (Events []) e = e
  (<>) e (Events []) = e
  (<>) e1 e2 = combineEvents e1 e2

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

newtype Probs = Probs [Double]

instance Semigroup Probs where
  (<>) (Probs []) p = p
  (<>) p (Probs []) = p
  (<>) p1 p2 = combineProbs p1 p2

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show :: PTable -> String
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    l2Length = length l2
    newL1 = mconcat $ map (replicate l2Length) l1
    -- nToAdd = length l2
    -- repeatedL1 = map (take nToAdd . repeat) l1
    -- newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events l1) (Events l2) = Events $ cartCombine (\x y -> mconcat [x, "-", y]) l1 l2

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable (PTable (Events []) (Probs [])) = ptable
  (<>) (PTable (Events []) (Probs [])) ptable = ptable
  (<>) (PTable e1 p1) (PTable e2 p2) = PTable newEvent newProbs
    where
      newEvent = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])