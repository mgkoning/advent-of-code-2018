import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HashMap

data Acre = Lumberyard | Trees | Open deriving (Show, Eq) 

type LumberArea = Map (Int, Int) Acre

(-:) x f = f x

readInput :: String -> LumberArea 
readInput input = lines input -: enumerated -: map coordinizeLine -: concat -: fromList
  where coordinizeLine (y, l) = zipWith combineCoord (repeat y) (map readAcre l -: enumerated)
        combineCoord y (x, c) = ((x, y), c)
        readAcre c = case c of
          '.' -> Open
          '|' -> Trees
          '#' -> Lumberyard

enumerated :: [a] -> [(Int, a)]
enumerated = zip [0..]

getNeighbors area (x, y) = catMaybes $ [Map.lookup (x + dx, y + dy) area | dx <- [-1..1], dy <- [-1..1], dy /= 0 || dx /= 0]

count n acreType = length $ filter (==acreType) n

magicHappens area = Map.mapWithKey determineNew area
  where determineNew pos now =
          let neighbors = getNeighbors area pos
              treesCount = count neighbors Trees
              lumberyardCount = count neighbors Lumberyard
          in case now of
            Open -> if 2 < treesCount then Trees else Open
            Trees -> if 2 < lumberyardCount then Lumberyard else Trees
            Lumberyard -> if 0 < lumberyardCount && 0 < treesCount then Lumberyard else Open

detectCycle magic = enumerated magic -: detectCycle' HashMap.empty
  where detectCycle' seen ((i, m):ms) =
          let areaMap = showMap m
              current = HashMap.lookup areaMap seen
          in case current of
            Nothing -> detectCycle' (HashMap.insert areaMap i seen) ms
            Just oldIndex -> (oldIndex, i)

areaValue area =
  let areaAsList = Map.elems area
      treesCount = count areaAsList Trees
      lumberyardCount = count areaAsList Lumberyard
  in treesCount * lumberyardCount

solve = do
  area <- readInput <$> readFile "input.txt"
  let infiniteMagic = iterate magicHappens area
  putStrLn "Part 1:"
  let after10Minutes = head $ drop 10 $ infiniteMagic
  putStrLn $ show $ areaValue after10Minutes
  putStrLn "Part 2:"
  let (from, to) = detectCycle infiniteMagic
  let target = 1000000000
  let variation = (target - from) `mod` (to - from)
  putStrLn $ show $ areaValue $ head $ drop (from + variation) $ infiniteMagic

testInput = unlines [
  ".#.#...|#.",
  ".....#|##|",
  ".|..|...#.",
  "..|#.....#",
  "#.#|||#|#|",
  "...#.||...",
  ".|....|...",
  "||...#|.#|",
  "|.||||..|.",
  "...#.|..|."]

showMap area =
  let (left, right, top, bottom) = (minimum xs, maximum xs, minimum ys, maximum ys)
      (xs, ys) = unzip $ Map.keys area
      makeLine y = [(makePos x y) | x <- [left..right]]
      makePos x y =
        let acre = area Map.! (x, y)
        in case acre of
          Open -> '.'
          Trees -> '|'
          Lumberyard -> '#'
  in unlines $ [makeLine y | y <- [top..bottom]]