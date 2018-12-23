import Data.List (sortOn, subsequences)

newtype Vector3 = Vector3 (Int, Int, Int) deriving (Show, Eq, Ord)

data Nanobot = Nanobot { pos :: Vector3, radius :: Int} deriving (Show, Eq, Ord)

readNanobot l =
  let (_, a) = break (=='<') l
      (b, c) = break (=='>') a
      pos = read $ "(" ++ (drop 1 b) ++ ")"
      r = read $ drop 5 c
  in Nanobot (Vector3 pos) r

readNanobots input = map readNanobot $ lines input

inRangeCount nanobots = length $ filter (inRange baddestBot) nanobots
  where baddestBot = head $ reverse $ sortOn radius nanobots
        inRange (Nanobot baddestPos radius) (Nanobot pos _) = distance baddestPos pos <= radius

distance (Vector3 (x, y, z)) (Vector3 (x', y', z')) = sum [abs $ x - x', abs $ y - y', abs $ z - z']

solve = do
  nanobots <- readNanobots <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ inRangeCount nanobots


testNanobots = map readNanobot [
  "pos=<10,12,12>, r=2",
  "pos=<12,14,12>, r=2",
  "pos=<16,12,12>, r=4",
  "pos=<14,14,14>, r=6",
  "pos=<50,50,50>, r=200",
  "pos=<10,10,10>, r=5"]

