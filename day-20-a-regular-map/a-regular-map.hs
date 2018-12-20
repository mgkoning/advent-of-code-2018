import Data.HashMap.Strict (insertWith)
import qualified Data.HashMap.Strict as Map
import Data.List (foldl', sortOn, groupBy)
import Data.Function (on)

start = ((0, 0), 0) :: ((Int, Int), Int)

buildMap s = move (dropWhile (=='^') s) [start] (Map.fromList [start])
  where move [] c mapSoFar = (mapSoFar, c, [])
        move (d:directions) currentPositions mapSoFar
          | d == '$' = (mapSoFar, currentPositions, directions)
          | d `elem` "NEWS" = let newPositions = map (step d) currentPositions
                                  m' = (updateMap mapSoFar newPositions)
                              in move directions newPositions m'
          | d == '(' = let (newPositions, remainingDirections, childrenMap) = followChildren directions currentPositions [] mapSoFar
                       in move remainingDirections (uniquify newPositions) childrenMap
          | otherwise = (mapSoFar, currentPositions, (d:directions))
        followChildren directions startingPositions positions mapSoFar =
          let (m', pos', d:ds) = move directions startingPositions mapSoFar
          in case d of
               ')' -> (pos' ++ positions, ds, m')
               '|' -> followChildren ds startingPositions (pos' ++ positions) m'
        updateMap = foldl' (\m (pos, l) -> insertWith min pos l m) 
        step d (p, l) = let p' = case d of 'N' -> up p; 'S' -> down p; 'W' -> left p; 'E' -> right p in (p', l+1)

uniquify positions = map (head . sortOn snd) $ groupBy ((==) `on` fst) $ sortOn fst positions

add (x, y) (dx, dy) = (x + dx,  y + dy)
up = (`add` (0, 1))
down = (`add` (0, -1))
left = (`add` (-1, 0))
right = (`add` (1, 0))

part1 = Map.foldl' max 0

part2 = length . filter ((>=1000) . snd) . Map.toList

solve = do
  (winner, _, _) <- buildMap <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 winner
  putStrLn "Part 2:"
  print $ part2 winner

testPart1 s = let (m, _, _) = buildMap s in part1 m