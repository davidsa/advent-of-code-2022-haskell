import Data.List (elemIndex)

main :: IO ()
main = do
  content <- readFile "./inputs/day3.txt"
  print (solve2 $ lines content)

solve1 :: [String] -> Int
solve1 xs =
  let common = findCommon "" xs
   in calcScore 0 common

score :: [Char]
score = ['a' .. 'z'] ++ ['A' .. 'Z']

calcScore :: Int -> String -> Int
calcScore acc input =
  case input of
    "" -> acc
    x : rest ->
      case getIndex x score of
        Just x -> calcScore (acc + x) rest
        Nothing -> calcScore acc rest

findCommon :: String -> [String] -> String
findCommon arr lines =
  case lines of
    [] -> arr
    x : rest ->
      findCommon (arr ++ [findIntersection x]) rest

findIntersection :: String -> Char
findIntersection str =
  let parts = splitHalf str
   in case parts of
        (xs, ys) -> xs `intersect` ys

splitHalf :: [a] -> ([a], [a])
splitHalf list = splitAt ((length list + 1) `div` 2) list

intersect :: String -> String -> Char
intersect xs = head . filter (`elem` xs)

getIndex :: Char -> String -> Maybe Int
getIndex x str =
  let maybe = x `elemIndex` str
   in fmap addOne maybe

addOne :: Int -> Int
addOne x = x + 1

----------- Part 2 -----------------

solve2 :: [String] -> Int
solve2 lines =
  let common = findCommon2 "" lines
   in calcScore 0 common

findCommon2 :: String -> [String] -> String
findCommon2 arr lines =
  case lines of
    [] -> arr
    x : y : z : rest ->
      findCommon2 (arr ++ [intersect3 x y z]) rest

intersect3 :: String -> String -> String -> Char
intersect3 xs ys =
  head . filter (`elem` ys) . filter (`elem` xs)
