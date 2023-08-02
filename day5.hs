main :: IO ()
main = do
  content <- readFile "./inputs/day5.txt"
  let first : last : _ = splitOnEmptyLines content
  print $ prepareLines $ reverseLines first

splitOnEmptyLines :: String -> [String]
splitOnEmptyLines = groupByEmptyLines . lines

groupByEmptyLines :: [String] -> [String]
groupByEmptyLines [] = []
groupByEmptyLines xs =
  let (chunk, rest) = break null xs
   in unlines chunk : groupByEmptyLines (dropWhile null rest)

prepareLines :: [String] -> [[String]]
prepareLines (x : xs) =
  let acc = initMatrix [] (getLength x)
   in iterLines acc xs

iterLines :: [[String]] -> [String] -> [[String]]
iterLines acc xs =
  case xs of
    [] -> acc
    (input : rest) ->
      let line = parseLine [] input
       in iterLines (concatMatrices line acc) rest

parseLine :: [[String]] -> String -> [[String]]
parseLine acc input =
  case input of
    "" -> reverse acc
    i ->
      let (x, y) = splitIntoTuple i
       in parseLine ([getLetterFromBox x] : acc) y

getLetterFromBox :: String -> String
getLetterFromBox "   " = []
getLetterFromBox ('[' : letter : ']' : _) = charToString letter

getLength :: String -> Int
getLength = length . words

reverseLines :: String -> [String]
reverseLines = reverse . lines

charToString :: Char -> String
charToString c = [c]

splitIntoTuple :: String -> (String, String)
splitIntoTuple str =
  let start = take 3 str
      end = trimStart $ drop 3 str
   in (start, end)

trimStart :: String -> String
trimStart str =
  case str of
    (' ' : rest) -> rest
    _ -> str

concatMatrices :: [[String]] -> [[String]] -> [[String]]
concatMatrices m1 m2 =
  [x ++ y | (i, x) <- zip [0 ..] m1, (j, y) <- zip [0 ..] m2, i == j]

initMatrix :: [[a]] -> Int -> [[a]]
initMatrix acc num =
  case num of
    0 -> acc
    _ -> initMatrix ([] : acc) (num - 1)
