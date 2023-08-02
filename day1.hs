import Data.List
import Text.Read

main :: IO ()
main = do
  content <- readFile "./inputs/day1.txt"
  print $ solve2 $ lines content

-- print (solve1 (lines content))

solve1 :: [String] -> Int
solve1 = maxVal . process [] 0

solve2 :: [String] -> Int
solve2 = sum . top3 . sortByGreater . process [] 0

process :: [Int] -> Int -> [String] -> [Int]
process result acc list =
  case list of
    -- Base2
    [] -> result
    -- Sum first with acc
    "" : rest ->
      process (acc : result) 0 rest
    x : rest ->
      process result (acc + toInt x) rest

maxVal :: [Int] -> Int
maxVal = maximum

toInt :: String -> Int
toInt = read

top3 :: [Int] -> [Int]
top3 = take 3

sortByGreater :: [Int] -> [Int]
sortByGreater = reverse . sort
