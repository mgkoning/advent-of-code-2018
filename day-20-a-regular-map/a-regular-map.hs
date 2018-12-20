import Data.Map.Strict (Map, insertWith)
import qualified Data.Map.Strict as Map

buildMap s = move (drop 1 s) [((0,0), 0)] Map.empty
  where move [] c mapSoFar = (mapSoFar, [])
        move (d:directions) currentPositions mapSoFar
          | d == '$' = (mapSoFar, directions)
          | d `elem` "NEWS" = let newPositions = map (step d) currentPositions
                              in move directions newPositions (updateMap mapSoFar newPositions)
          | d == '(' = let (newPositions, remainingDirections, childrenMap) = followChildren directions currentPositions [] mapSoFar
                       in move remainingDirections newPositions childrenMap
        followChildren directions startingPositions positions mapSoFar = 
          let 1 = 1 in (positions, directions, mapSoFar)
          -- | d == ')' = (positions, directions, mapSoFar)
          -- | d == '|' = 
        updateMap = foldr (\(pos, l) m -> insertWith min pos l m)
        step d (p, l) = let l' = l+1; p' = case d of 'N' -> up p; 'S' -> down p; 'W' -> left p; 'E' -> right p in l' `seq` (p', l')

add (x, y) (dx, dy) = let x' = x + dx; y' = y + dy in x' `seq` y' `seq` (x', y')
up = (`add` (0, 1))
down = (`add` (0, -1))
left = (`add` (-1, 0))
right = (`add` (1, 0))

solve = do
  (builtMap, _) <- buildMap <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ Map.foldl max 0 builtMap