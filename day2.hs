main :: IO ()
main = do
  content <- readFile "./inputs/day2.txt"
  print (solve1 0 (lines content))

data Move
  = Rock
  | Paper
  | Scissor

data Outcome
  = Win
  | Draw
  | Lose

solve1 :: Int -> [String] -> Int
solve1 acc list =
  case list of
    [] -> acc
    (opponent : ' ' : you : _) : rest ->
      solve1
        (acc + calculateRound (parseMove opponent) (parseOutcome you))
        rest

parseMove :: Char -> Move
parseMove str =
  case str of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissor

parseOutcome :: Char -> Outcome
parseOutcome str =
  case str of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win

calculateRound :: Move -> Outcome -> Int
calculateRound a b =
  case a of
    Rock ->
      case b of
        Win ->
          2 + 6 -- Paper
        Draw ->
          1 + 3 -- Rock
        Lose ->
          3 + 0 -- Scissor
    Paper ->
      case b of
        Win ->
          3 + 6 -- Scissor
        Draw ->
          2 + 3 -- Paper
        Lose ->
          1 + 0 -- Rock
    Scissor ->
      case b of
        Win ->
          1 + 6 -- Rock
        Draw ->
          3 + 3 -- Scissor
        Lose ->
          2 + 0 -- Paper
