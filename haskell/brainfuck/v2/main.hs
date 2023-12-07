import Data.Array
import Data.Char (chr)

type Index = Int
type Depth = Int

data Brain = Brain
  { memory :: [Int],
    pointer :: Int,
    arrayIndex :: Int,
    memoryIndex :: Int
  }
  deriving (Show)

-- convert from string to array
stringToArray :: String -> Array Int Char
stringToArray str = listArray (0, length str - 1) str

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAtIndex (n - 1) newVal xs

modifyMemory :: Brain -> [Int]
modifyMemory brain
  | memIndex >= 0 && memIndex < length (memory brain) =
      replaceAtIndex memIndex (pointer brain) (memory brain)
  | otherwise = memory brain
  where
    memIndex = memoryIndex brain

run :: Array Int Char -> Brain -> IO ()
run array brain
  -- read all input
  | arrayIndex brain > length array - 1 = putChar '\0'
  -- memoryIndex is not negative
  | memoryIndex brain < 0 = putStrLn "Runtime error: Jumped beyond list bounds"
  | otherwise  = do
      let token = array ! arrayIndex brain
      case token of
        '+' -> run array (brain {pointer = pointer brain + 1, arrayIndex = arrayIndex brain + 1})
        '-' -> run array (brain {pointer = pointer brain - 1, arrayIndex = arrayIndex brain + 1})
        '>' ->
          run
            array
            ( brain
                { memory = modifyMemory brain,
                  memoryIndex = nextMemoryIndex,
                  arrayIndex = arrayIndex brain + 1,
                  pointer = memory brain !! nextMemoryIndex
                }
            )
            where nextMemoryIndex = memoryIndex brain + 1
        '<' ->
          run
            array
            ( brain
                { memory = modifyMemory brain,
                  memoryIndex = prevMemoryIndex,
                  arrayIndex = arrayIndex brain + 1,
                  pointer = memory brain !! prevMemoryIndex

                }
            )
            where prevMemoryIndex = memoryIndex brain - 1
        '.' -> do
          putChar $ chr (pointer brain)
          run array (brain {arrayIndex = arrayIndex brain + 1})
        '[' -> case pointer brain of
          0 -> do
            let depth = 1
            jumpForward array brain depth
          _ -> run array (brain {arrayIndex = arrayIndex brain + 1})
        ']' -> case pointer brain of
          0 -> run array (brain {arrayIndex = arrayIndex brain + 1})
          _ -> do
            let depth = 1
            jumpBackward array brain depth
        '\n' -> run array brain

jumpForward :: Array Int Char -> Brain -> Depth -> IO ()
jumpForward array brain depth
  | arrayIndex brain >= length array = putStrLn "Runtime error: Jumped beyond array bounds"
  | depth == 0 = run array brain
  | otherwise =
    let
      nextIndex = arrayIndex brain + 1
      nextToken = array ! nextIndex
      newDepth = case nextToken of
                    '[' -> depth + 1
                    ']' -> depth - 1
                    _   -> depth
      newBrain = brain { arrayIndex = nextIndex }
    in jumpForward array newBrain newDepth

jumpBackward :: Array Int Char -> Brain -> Depth -> IO ()
jumpBackward array brain depth
  | arrayIndex brain < 0 = putStrLn "Runtime error: Jumped beyond array bounds"
  | depth == 0 = run array brain
  | otherwise =
    let
      prevIndex = arrayIndex brain - 1
      prevToken = array ! prevIndex
      newDepth = case prevToken of
                    ']' -> depth + 1
                    '[' -> depth - 1
                    _   -> depth
      newBrain = brain { arrayIndex = prevIndex }
    in jumpBackward array newBrain newDepth



main :: IO ()
main = do
  input <- getLine
  let brain =
        Brain
          { memory = replicate 100 0,
            pointer = 0,
            arrayIndex = 0,
            memoryIndex = 0
          }
  run (stringToArray input) brain