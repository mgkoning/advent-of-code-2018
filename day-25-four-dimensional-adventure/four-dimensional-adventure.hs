import CommonHs.Parsing
import Text.Parsec (sepEndBy, parse, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type FixedPoint = (Int, Int, Int, Int)

parseFixedPoints :: String -> [FixedPoint]
parseFixedPoints input = case parse (parseFixedPoint `sepEndBy` eol <* eof) "" input of
                     Left msg -> error (show msg)
                     Right r -> r

parseFixedPoint :: Parser FixedPoint
parseFixedPoint = (,,,) <$> parseInt <* char ',' <*> parseInt <* char ',' <*> parseInt <* char ',' <*> parseInt

distance :: FixedPoint -> FixedPoint -> Int
distance (x1, y1, z1, t1) (x2, y2, z2, t2) = sum [abs $ x1-x2, abs $ y1-y2, abs $ z1-z2, abs $ t1-t2]

findConstellations fixedPoints = findConstellations' (Set.fromList fixedPoints) []
  where graph = buildGraph fixedPoints
        findConstellations' remainingPoints found
          | Set.null remainingPoints = found
          | otherwise = findConstellations' remainingPoints' found'
              where (next, withoutNext) = Set.deleteFindMin remainingPoints
                    constellation = constellationFrom next graph
                    remainingPoints' = withoutNext Set.\\ constellation
                    found' = constellation:found

constellationFrom startingPoint graph = buildConstellation (Set.singleton startingPoint) Set.empty
  where buildConstellation toDo found
          | Set.null toDo = found
          | otherwise = let (next, toDo') = Set.deleteFindMin toDo
                            newNeighbors = (graph Map.! next) Set.\\ found
                            found' = found `Set.union` newNeighbors
                        in buildConstellation (toDo' `Set.union` newNeighbors) (Set.insert next found')

buildGraph fixedPoints = buildGraph' fixedPoints Map.empty
  where buildGraph' [] graph = graph
        buildGraph' (f:fs) graph = buildGraph' fs graph'
          where neighbors = findNeighbors f fs
                graphWithF = Map.insertWith Set.union f (Set.fromList $ findNeighbors f fs) graph
                graph' = foldr (\n -> Map.insertWith Set.union n (Set.singleton f)) graphWithF neighbors

findNeighbors f fs = filter ((<=3) . (distance f)) fs

solve = do
  fixedPoints <- parseFixedPoints <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ length $ findConstellations fixedPoints


testInput0 = unlines ["0,0,0,0","3,0,0,0","0,3,0,0","0,0,3,0","0,0,0,3","0,0,0,6","9,0,0,0","12,0,0,0"]
testInput1 = unlines ["-1,2,2,0","0,0,2,-2","0,0,0,-2","-1,2,0,0","-2,-2,-2,2","3,0,2,-1","-1,3,2,2","-1,0,-1,0","0,2,1,-2","3,0,0,0"]
testInput2 = unlines ["1,-1,0,1","2,0,-1,0","3,2,-1,0","0,0,3,1","0,0,-1,-1","2,3,-2,0","-2,2,0,0","2,-2,0,-1","1,-1,0,-1","3,2,0,2"]
testInput3 = unlines ["1,-1,-1,-2","-2,-2,0,1","0,2,1,3","-2,3,-2,1","0,2,3,-2","-1,-1,1,-2","0,-2,-1,0","-2,2,3,-1","1,2,2,0","-1,-2,0,-2"]
