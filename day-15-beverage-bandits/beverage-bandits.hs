import Prelude hiding (round)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Function (on)
import Data.Tuple (swap)
import Data.Maybe (fromJust, isJust, listToMaybe)

data Creature = Creature { creatureId :: Integer, faction :: Faction, hp :: Integer, position :: Position, ap :: Integer } deriving (Show)
creatureName c = (show $ faction c) ++ " " ++ (show $ creatureId c)

data Faction = Elf | Goblin  deriving (Show, Eq)

data GameState = GameState { gameMap :: GameMap, creatures :: [Creature], done :: Bool } deriving (Show)

type Position = (Integer, Integer)
type GameMap = Set Position

{- This 'pipeline operator' allows left-to-right writing of function applications
   to help readability of function chains. -}
(-:) :: a -> (a -> b) -> b
(-:) x f = f x

readInput :: String -> GameState 
readInput input = GameState gameMap creatures False
  where withCoordinates = lines input -: enumerated -: map coordinizeLine -: concat -: dropWalls
        coordinizeLine (y, l) = zipWith combineCoord (repeat y) (enumerated l)
        combineCoord y (x, c) = ((x, y), c)
        dropWalls = dropAll '#'
        dropEmpty = dropAll '.'
        gameMap = map fst withCoordinates -: Set.fromList
        creatures = dropEmpty withCoordinates -: enumerated -: map toCreature
        toCreature (id, (xy, c)) = Creature id (toFaction c) 200 xy 3

dropAll c = filter ((/=c) . snd)

toFaction c = case c of
  'E' -> Elf
  'G' -> Goblin
  _ -> error ("Unknown faction " ++ (show c))

enumerated :: [a] -> [(Integer, a)]
enumerated = zip [0..]

round state = let s' = foldl' turn state sortedCreatures in s'
  where sortedCreatures = map creatureId $ sortOn (swap . position) (creatures state)

turn state id = if null foundCreature then state {- This creature may have been eliminated before its turn -}
                else if null targets then state { done = True }
                else doAttack afterMove creature' targets
  where foundCreature = filter ((id==) . creatureId) (creatures state) 
        creature = head foundCreature
        targets = findTargets state (faction creature)
        newPosition = determineMove state creature targets
        creature' = creature { position = newPosition }
        afterMove = state { creatures = replaceCreature (creatures state) creature' }

replaceCreature cs c = c:(removeCreature cs c)

removeCreature cs c = (filter ((/= (creatureId c)) . creatureId) cs)

determineMove state creature targets = if targetInRange || null shortestPaths then currentPos else move
  where currentPos = position creature
        targetInRange = not $ null $ inRange currentPos targets
        targetLocations = sortOn (distanceTo currentPos) $ concatMap ((emptyNeighbors state) . position) targets
        shortestPaths = findShortestPaths state currentPos (Set.fromList targetLocations)
        actualTarget = head $ groupBy ((==) `on` fst) $ sortOn (swap . fst) shortestPaths
        move = head $ sortOn swap $ map (head . snd) actualTarget

inRange pos targets = filter ((isAdjacent pos) . position) targets

doAttack state creature targets = if null targetsInRange then state else state'
  where currentPos = position creature
        targetsInRange = inRange currentPos targets
        actualTarget = head $ sortOn (swap . position) $ head $ groupBy ((==) `on` hp) $ sortOn hp targetsInRange
        target' = takeDamage actualTarget (ap creature)
        creatures' = if dead target' then removeCreature (creatures state) target' else replaceCreature (creatures state) target'
        state' = state { creatures = creatures' }

takeDamage creature damage = creature { hp = (hp creature) - damage }

dead creature = hp creature <= 0

isAdjacent (x1, y1) (x2, y2) = distanceTo (x1, y1) (x2, y2) == 1

distanceTo (x1, y1) (x2, y2) = (abs $ x1-x2) + (abs $ y1-y2)

tplus (x1, y1) (x2, y2) = let x' = x1 + x2; y' = y1 + y2 in x' `seq` y' `seq` (x', y')

adjacentSquares p = map (tplus p) [(0,-1), (-1,0), (1,0), (0,1)]

findTargets (GameState _ creatures _) itsFaction = filter ((/=itsFaction) . faction) creatures

emptyNeighbors (GameState gameMap creatures _) pos = result
  where result = filter (`Set.member` gameMap) (adjacentSquares pos) \\ map position creatures

findShortestPaths state source targets =
      shortestPaths' (zip (emptyNeighbors state source) (repeat [])) (Set.singleton source) (Set.singleton source) []
  where shortestPaths' [] _ _ foundPaths = foundPaths
        shortestPaths' ((pos, pathTaken):ps) visited queued foundPaths
          | pos `Set.member` visited = shortestPaths' ps visited' queued foundPaths
          | (not $ pos `Set.member` targets) && null foundPaths = shortestPaths' toVisit visited' queued' foundPaths
          | pos `Set.member` targets && null foundPaths = shortestPaths' ps visited' queued [(pos, reverse (newPath))]
          | (length newPath) > firstPathLength = foundPaths
          | pos `Set.member` targets = shortestPaths' ps visited' queued ((pos, reverse (newPath)):foundPaths)
          | otherwise = shortestPaths' ps visited' queued foundPaths
              where newPath = pos:pathTaken
                    visited' = Set.insert pos visited
                    queued' = foldr Set.insert queued unvisitedNeighbors
                    unvisitedNeighbors = filter (not . (`Set.member` queued)) $ filter (not . (`Set.member` visited')) (emptyNeighbors state pos)
                    nextMoves = (zip unvisitedNeighbors (repeat (pos:pathTaken)))
                    firstPathLength = length $ head foundPaths
                    toVisit = (ps ++ nextMoves)


fightUntilDone state = head $ dropWhile (\s -> not $ done $ snd s) $ zip [0..] (iterate round state)

part1 state =  let result = fightUntilDone state in (fst result - 1) * (sum $ map hp $ creatures $ snd result)

part2 state = snd $head $ dropWhile (not . fst) $ map (changeOdds state) [4..]

changeOdds state elfPower = 
  let elfs = filter isElf (creatures state)
      isElf c = faction c == Elf
      elfCount = length elfs
      elfPowerUp c =  if isElf c then c { ap = elfPower } else c
      state' = state { creatures = map elfPowerUp (creatures state) }
      (rounds, afterMath) = fightUntilDone state'
      remainingCreatures = creatures afterMath
      score = (rounds - 1) * (sum $ map hp $ remainingCreatures)
  in ((length $ filter isElf remainingCreatures) == elfCount, score)

solve = do
  state <- readInput <$> readFile "input.txt"
  putStrLn "Part 1:"
  let result = part1 state
  putStrLn $ show $ result
  putStrLn "Part 2:"
  let result = part2 state
  putStrLn $ show $ result


testState = readInput testInput
GameState testMap testCreatures _ = testState
testInput =
  "#######\n" ++
  "#.G...#\n" ++ 
  "#...EG#\n" ++ 
  "#.#.#G#\n" ++ 
  "#..G#E#\n" ++ 
  "#.....#\n" ++
  "#######\n"

testInput2 =
  "#######\n" ++
  "#G..#E#\n" ++
  "#E#E.E#\n" ++
  "#G.##.#\n" ++
  "#...#E#\n" ++
  "#...E.#\n" ++
  "#######\n"

testInput3 =
  "#######\n" ++
  "#E..EG#\n" ++
  "#.#G.E#\n" ++
  "#E.##E#\n" ++
  "#G..#.#\n" ++
  "#..E#.#\n" ++
  "#######\n"


showState (GameState gameMap creatures _) =
  let (right, bottom) = (maximum xs, maximum ys)
      xs = Set.map fst gameMap
      ys = Set.map snd gameMap
      makeLine y = [(makePos x y) | x <- [0..right+1]]
      creatureAt pos = listToMaybe $ map (\c -> if faction c == Elf then 'E' else 'G') $ filter ((== pos) . position) creatures
      makePos x y = let creature = creatureAt (x,y) in if isJust $ creature then fromJust creature else if (x,y) `Set.member` gameMap then ' ' else '+'
  in unlines $ [line | y <- [0..bottom+1], let line = makeLine y]
