import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Position = (Int, Int)
data Terrain = Rocky | Wet | Narrow deriving (Show, Eq)
data Equipment = Torch | ClimbingGear | Neither deriving (Show, Eq, Ord)

data Region = Region { erosionLevel :: Int, terrain :: Terrain } deriving (Show)
data State = State { bestCase :: Int, timeTaken :: Int, position :: Position, equipped :: Equipment } deriving (Show, Eq, Ord)

allowedEquipment Rocky = [Torch, ClimbingGear]
allowedEquipment Wet = [ClimbingGear, Neither]
allowedEquipment Narrow = [Torch, Neither]

regionAt depth cave at@(x,y)
  | at `HashMap.member` cave = (cave ! at, cave)
  | x == 0 = let gi = 48271 * y; newRegion = makeRegion gi depth in (newRegion, HashMap.insert at newRegion cave)
  | y == 0 = let gi = 16807 * x; newRegion = makeRegion gi depth in (newRegion, HashMap.insert at newRegion cave)
  | otherwise = let (left, leftCave) = regionAt depth cave (x-1, y)
                    (right, rightCave) = regionAt depth leftCave (x, y-1)
                    newRegion = makeRegion ((erosionLevel left) * (erosionLevel right)) depth
                in (newRegion, HashMap.insert at newRegion rightCave)
    
makeRegion gi depth = let el = erosionLevelAt gi depth
                      in Region el $ case el `mod` 3 of 0 -> Rocky; 1 -> Wet; 2 -> Narrow
erosionLevelAt gi depth = (gi + depth) `mod` 20183

buildMaze depth target = foldl' addToMaze baseMaze [(x, y) | x <- [0..fst target], y <- [0..snd target]]
  where baseMaze = HashMap.fromList [((0, 0), makeRegion 0 depth), (target, makeRegion 0 depth)]
        addToMaze c pos = snd $ regionAt depth c pos

part1 maze = HashMap.foldl' (+) 0 $ HashMap.map ((`mod` 3) . erosionLevel) maze

add (x, y) (dx, dy) = (x + dx,  y + dy)
up = (`add` (0, 1))
down = (`add` (0, -1))
left = (`add` (-1, 0))
right = (`add` (1, 0))

distance (fromX, fromY) (toX, toY) = abs (fromX - toX) + abs (fromY - toY)

isSolidRock (x, y) = x < 0 || y < 0

neighbors depth maze at = foldl' addNeighbor ([], maze) reachableNeighbors
  where reachableNeighbors = filter (not . isSolidRock) $ [up, down, left, right] <*> [at]
        addNeighbor (ns, m) n = let (r, m') = regionAt depth m n in ((n, r):ns, m')

moves eq nextRegion = addTime eq $ allowedEquipment nextRegion

addTime eq = map (\eq' -> (1 + if eq == eq' then 0 else 7, eq'))

part2 maze depth target = bestPath maze (Set.singleton startState) (Set.empty)
  where startState = State (distance (0,0) target) 0 (0,0) Torch
        bestPath maze toVisit visited
          | Set.null toVisit = error "Couldn't find target"
          | otherwise =
              let ((State _ time pos equipped), v) = Set.deleteFindMin toVisit
                  (n, maze') = neighbors depth maze pos
                  possibleMoves = concatMap constructMove $ filter validWithCurrentEquipment $ n
                  validWithCurrentEquipment (_, Region _ t) = equipped `elem` (allowedEquipment t)
                  constructMove (pos, Region _ t) = zip (repeat pos) (moves equipped t)
                  newStates = map constructState possibleMoves
                  constructState (pos, (extraTime, eq)) = let nextTime = time + extraTime
                                                              bestCase = nextTime + distance pos target
                                                          in State bestCase nextTime pos eq
                  toVisit' = foldr Set.insert v newStates
              in if pos == target && equipped == Torch then time
                 else if pos == target then bestPath maze v visited -- Invalid end state, needed Torch
                 else if (pos, equipped) `Set.member` visited then bestPath maze v visited
                 else bestPath maze' toVisit' (Set.insert (pos, equipped) visited)

{- Inputs -}
actualDepth = 4848
actualTarget = (15, 700) :: Position

testDepth = 510 :: Int
testTarget = (10, 10) :: Position

solve = do
  let maze = buildMaze actualDepth actualTarget
  putStrLn "Part 1:"
  print $ part1 maze
  putStrLn "Part 2:"
  print $ part2 maze actualDepth actualTarget