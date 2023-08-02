main :: IO ()
main = do
  content <- readFile "./inputs/day4.txt"
  print $ solve2 0 $ lines content

solve1 :: Int -> [String] -> Int
solve1 acc lines =
  case lines of
    [] -> acc
    input : rest ->
      let parts = splitOnChar ',' input
       in case parts of
            left : right : _ ->
              let x1 : y1 : _ = splitOnChar '-' left
                  x2 : y2 : _ = splitOnChar '-' right
               in solve1 (acc + calcScore (toInt x1, toInt x2) (toInt y1, toInt y2)) rest

calcScore :: (Int, Int) -> (Int, Int) -> Int
calcScore (x1, x2) (y1, y2) =
  if (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1)
    then 1
    else 0

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = []
splitOnChar c xs =
  let (before, rest) = break (== c) xs
   in before : case rest of
        [] -> []
        (_ : rs) -> splitOnChar c rs

toInt :: String -> Int
toInt = read

----------- Part 2 -----------------

solve2 :: Int -> [String] -> Int
solve2 acc lines =
  case lines of
    [] -> acc
    input : rest ->
      let parts = splitOnChar ',' input
       in case parts of
            left : right : _ ->
              let x1 : y1 : _ = splitOnChar '-' left
                  x2 : y2 : _ = splitOnChar '-' right
               in solve2 (acc + calcScore2 (toInt x1, toInt x2) (toInt y1, toInt y2)) rest

calcScore2 :: (Int, Int) -> (Int, Int) -> Int
calcScore2 (x1, x2) (y1, y2) =
  if (x1 >= x2 && x1 <= y2) || (x2 >= x1 && x2 <= y1)
    then 1
    else 0
