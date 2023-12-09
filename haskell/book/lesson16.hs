type Square = Float
type Rectangle = (Float, Float)
type Circle = Float


data Shape = Circle Float | Square Float | Rectangle (Float, Float)

circumferenceLength :: Shape -> Float
circumferenceLength (Circle radius) = pi * radius
circumferenceLength (Square side) = side * side
circumferenceLength (Rectangle (width, height)) = 2 * (width + height)