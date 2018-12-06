import Data.List (sortOn, sort, group, groupBy)
import Data.Map.Strict (fromList, toList)

type Coordinate = (Int, Int)
data ChronalCoordinate = ChronalCoordinate { chronalId :: Int, coordinate :: Coordinate } deriving (Show, Eq)

toCoords :: [String] -> [ChronalCoordinate]
toCoords lines = zipWith ChronalCoordinate [1..] $ map read $ map (\l -> "(" ++ l ++ ")") lines

borders coords = let allCoordinates = map coordinate coords
                     xs = map fst allCoordinates
                     ys = map snd allCoordinates
                 in (minimum xs, maximum ys, minimum ys, maximum xs)

distances coords point = map (manhattanDistance point) coords
  where manhattanDistance (x, y) (ChronalCoordinate id (a, b)) = (id, (abs (x - a) + abs (y - b)))

gridWithDistances coords = fromList $ zip gridCoords $ map (distances coords) gridCoords
  where 
    (left, top, bottom, right) = borders coords
    gridCoords = [(x, y) | x <- [left..right], y <- [bottom..top]]

part1 coords = winner
  where grid = toList $ gridWithDistances coords
        distances = map snd $ grid
        closestByPoint = filter ((==1) . length) $ map head $ map (groupBy (\x y -> snd x == snd y) . sortOn snd) $ distances
        winner = length $ head $ reverse $ sortOn length $ group $ sort $ map (fst . head) $  closestByPoint

part2 maxDist coords = totalPoints
  where totalPoints = length $ filter ((<maxDist) . sum . (map snd . snd)) $ toList $ gridWithDistances coords

solve = do
  coords <- toCoords <$> lines <$> readFile "input.txt"
  putStrLn "Part 1"
  let largestSurface = part1 coords
  putStrLn $ show largestSurface
  putStrLn "Part 2"
  let targetRegion = part2 10000 coords
  putStrLn $ show targetRegion

testCoords = 
  ["1, 1",
  "1, 6",
  "8, 3",
  "3, 4",
  "5, 5",
  "8, 9"]