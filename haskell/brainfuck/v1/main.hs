import Data.Char (chr)

data BFMemory = BFMemory
  { currentValue :: Int,
    cursor :: Int,
    memory :: [Int],
    loopCount :: Int
  }
  deriving (Show)

type BFSymbol = String

process :: BFMemory -> BFSymbol -> IO ()
process bfmemory "" = do
  putChar '\n'
  {--debug--}
  print bfmemory
process bfmemory ('+' : xs) = process (bfmemory {currentValue = currentValue bfmemory + 1}) xs
process bfmemory ('-' : xs) = process (bfmemory {currentValue = currentValue bfmemory - 1}) xs
process bfmemory ('.' : xs) = do
  putChar $ chr (currentValue bfmemory)
  process bfmemory xs
process bfmemory ('>' : xs) =
  process
    ( bfmemory
        { currentValue = memory bfmemory !! nextCursor,
          cursor = nextCursor,
          memory = replaceAtIndex (cursor bfmemory) (currentValue bfmemory) (memory bfmemory)
        }
    )
    xs
  where
    nextCursor = cursor bfmemory + 1
process bfmemory ('<' : xs) =
  process
    ( bfmemory
        { currentValue = memory bfmemory !! prevCusror,
          cursor = prevCusror,
          memory = replaceAtIndex (cursor bfmemory) (currentValue bfmemory) (memory bfmemory)
        }
    )
    xs
  where
    prevCusror = cursor bfmemory - 1

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAtIndex (n - 1) newVal xs

main :: IO ()
main = do
  input <- getLine
  let memory =
        BFMemory
          { currentValue = 0,
            cursor = 0,
            memory = replicate 100 0,
            loopCount = 0
          }
  process memory input