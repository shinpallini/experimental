data Color = Red | Yellow | Blue | Clear deriving (Show, Eq)

instance Semigroup Color where
    (<>) Clear any = any
    (<>) any Clear = any
    (<>) a b = a