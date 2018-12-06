import Data.List (sortOn, sort, group)
import Data.Map.Strict (Map, empty, insertLookupWithKey, toList)
import Data.Maybe (fromJust)

type ClosestChronal = (Int, Int)
type Coordinate = (Int, Int)
type VoronoiMap = Map (Int, Int) ClosestChronal
data ChronalCoordinate = ChronalCoordinate { chronalId :: Int, coordinate :: Coordinate } deriving (Show, Eq)


toCoords :: [String] -> [ChronalCoordinate]
toCoords lines = zipWith ChronalCoordinate [1..] $ map read $ map (\l -> "(" ++ l ++ ")") lines

neighboursAt n (x, y) = [(x + x', y + y') | x' <- [-n..n], y' <- [-n..n], abs x' + abs y' == n]

buildVoronoiMap :: [ChronalCoordinate] -> VoronoiMap
buildVoronoiMap coords = buildMap sortedCoords empty
  where 
    (left, top, bottom, right) = let allCoordinates = map coordinate coords
                                     xs = map fst allCoordinates
                                     ys = map snd allCoordinates
                                 in (minimum xs, maximum ys, minimum ys, maximum xs)
    sortedCoords = sortOn coordinate coords
    withinBounds (x, y) = x >= left && x <= right && y >= bottom && y <= top
    buildMap :: [ChronalCoordinate] -> VoronoiMap -> VoronoiMap
    buildMap [] m = m
    buildMap (c:cs) map = buildMap cs (fillMap c map)
    fillMap (ChronalCoordinate id coord) map = fillMap' 0 map
      where
        fillMap' n map =
          let neighbours = filter withinBounds $ neighboursAt n coord
              putInMap (m, c) neighbour =
                let
                  (prev, map) = insertLookupWithKey update neighbour (id, n) m
                  update key newValue oldValue
                    | snd oldValue == snd newValue = (-1, snd oldValue)
                    | snd oldValue < snd newValue = oldValue
                    | otherwise = newValue
                in (map, c + if prev == Nothing || snd (fromJust prev) >= n then 1 else 0)
              (newMap, inserted) = foldl putInMap (map, 0) neighbours
          in if inserted > 0 then fillMap' (n+1) newMap else map

part1 voronoiMap = length $ head $ reverse $ sortOn length $ group $ sort $ filter (>0) $ map fst $ map snd $ toList voronoiMap

solve = do
  putStrLn "Part 1"
  coords <- toCoords <$> lines <$> readFile "input.txt"
  let voronoiMap = buildVoronoiMap coords
  let largestSurface = part1 voronoiMap
  putStrLn $ show largestSurface

testCoords = 
  ["1, 1",
  "1, 6",
  "8, 3",
  "3, 4",
  "5, 5",
  "8, 9"]