import Data.Array
import Data.Char (chr)

type Index = Int

data Brain = Brain
  { memory :: [Int],
    pointer :: Int,
    arrayIndex :: Int,
    memoryIndex :: Int
  } deriving (Show)

-- 文字列をArrayに変換する関数
stringToArray :: String -> Array Int Char
stringToArray str = listArray (0, length str - 1) str

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAtIndex (n - 1) newVal xs

modifyMemory :: Brain -> [Int]
modifyMemory brain = replaceAtIndex (memoryIndex brain) (pointer brain) (memory brain)

run :: Array Int Char -> Brain -> IO ()
run array brain = do
  let token = array ! arrayIndex brain
  case token of
    '+' -> run array (brain {pointer = pointer brain + 1, arrayIndex = memoryIndex brain + 1})
    '-' -> run array (brain {pointer = pointer brain - 1, arrayIndex = memoryIndex brain + 1})
    '>' ->
      run
        array
        ( brain
            { memory = modifyMemory brain,
              memoryIndex = memoryIndex brain + 1,
              arrayIndex = memoryIndex brain + 1
            }
        )
    '<' ->
      run
        array
        ( brain
            { memory = modifyMemory brain,
              memoryIndex = memoryIndex brain - 1,
              arrayIndex = memoryIndex brain + 1
            }
        )
    '.' -> do
        print brain
        putChar $ chr (pointer brain)
    

main :: IO ()
main = do
  input <- getLine
  putChar $ stringToArray input ! 1