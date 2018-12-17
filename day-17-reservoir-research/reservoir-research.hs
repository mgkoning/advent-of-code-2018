import Data.Set (fromList, toList, empty, member, union, unions, insert, delete, size)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.List (foldl')

data Obstruction = Clay | Flooded deriving (Show, Eq)

lineToClayCoordinates line = clayCoordinates
  where (lineAt, rest) = span (/=',') line
        xOrY = head lineAt
        xOrYCoord = read $ drop 2 lineAt :: Int
        lineFromTo = drop 4 rest
        (lineFrom, a) = span (/='.') lineFromTo
        lineTo = dropWhile (=='.') a
        clayCoordinates = (if xOrY == 'y' then flip zip else zip) (repeat xOrYCoord) [(read lineFrom)..(read lineTo)]

inputToClayCoordinates input = fromList $ concatMap lineToClayCoordinates $ lines input

up    (x, y) = (x,   y-1)
down  (x, y) = (x,   y+1)
left  (x, y) = (x-1, y)
right (x, y) = (x+1, y)

springEternal clay = flowFrom' (500, 0)  empty empty
  where absoluteBottom = maximum $ Set.map snd clay
        flowFrom' from wetTiles flooded =
          let (obstruction, path) = trickleDown absoluteBottom from clay flooded
              wetTilesDown = wetTiles `union` (fromList path)
          in case obstruction of
            Nothing -> (wetTilesDown, flooded) -- fell out the bottom
            Just (coord, obsType) ->
              let (leftWayDown, leftPath) = flowSideWays coord left clay flooded
                  (rightWayDown, rightPath) = flowSideWays coord right clay flooded
                  confined = leftWayDown == Nothing && rightWayDown == Nothing
                  wetTilesSideways = insert coord $ Set.unions [wetTilesDown, fromList leftPath, fromList rightPath]
                  waysDown = filter (not . (`member` wetTiles)) $ catMaybes [leftWayDown, rightWayDown]
              in if confined
                       {- flood this level and go back up -}
                  then flowFrom' (up coord) (delete coord wetTilesDown) (insert coord $ Set.unions [flooded, fromList leftPath, fromList rightPath])
                       {- recurse the algorithm from the sides that actually have a way down and weren't visited before -}
                  else foldl' (\(wet, flooded) f -> flowFrom' f wet flooded) (wetTilesSideways, flooded) waysDown

{- Flow sideways and report whether a way down can be found -}
flowSideWays from direction clay flooded = flowSideWays' from []
  where flowSideWays' from path
          | (not $ under `member` clay) && (not $ under `member` flooded) = (Just from, path)
          | next `member` clay = (Nothing, path)
          | otherwise = flowSideWays' next (next:path)
          where next = direction from
                under = down from

{- Go down as far as possible and report whether an obstruction was found there -}
trickleDown bottom from clay flooded = trickleDown' from []
  where trickleDown' from path
          | nextY > bottom = (Nothing, path)
          | next `member` clay = (Just (from, Clay), path)
          | next `member` flooded = (Just (from, Flooded), path)
          | otherwise = trickleDown' next (next:path)
          where next = down from
                nextY = snd next

{- Calculate answer for part 1 -}
wetAndFloodedTiles minY flooded wet = size $ Set.filter (\(_, y) -> y >= minY) $ flooded `union` wet

solve = do
  clay <- inputToClayCoordinates <$> readFile "input.txt"
  let minY = minimum $ Set.map snd clay
  putStrLn "Part 1:"
  let (wet, flooded) = springEternal clay
  let answerP1 = wetAndFloodedTiles minY flooded wet
  --putStrLn $ showMap clay wet flooded
  putStrLn $ show $ answerP1
  putStrLn "Part 2:"
  putStrLn $ show $ size flooded

testPart1 =
  let clay = inputToClayCoordinates testInput
      minY = minimum $ Set.map snd clay
      (w, f) = springEternal clay
      answer = wetAndFloodedTiles minY w f
  in answer --showMap clay w f

testInput = unlines [
  "x=495, y=2..7",
  "y=7, x=495..501",
  "x=501, y=3..7",
  "x=498, y=2..4",
  "x=506, y=1..2",
  "x=498, y=10..13",
  "x=504, y=10..13",
  "y=13, x=498..504"]

printInputMap = do
  coords <- inputToClayCoordinates <$> readFile "input.txt"
  putStrLn $ showMap coords empty empty

printTestInputMap = do
  let coords = inputToClayCoordinates testInput
  putStrLn $ showMap coords empty empty

{- This was a life saver, also fun to see -}
showMap clay wet flooded =
  let (left, right, top, bottom) = (minimum xs, maximum xs, minimum ys, maximum ys)
      (xs, ys) = unzip $ toList clay
      makeLine y = [(makePos x y) | x <- [left-1..right+1]]
      makePos x y =
        if (x, y) == (500, 0) then '+'
        else if (x,y) `member` flooded then '~'
        else if (x,y) `member` wet then '|'
        else if (x,y) `member` clay then '#'
        else '.'
  in unlines $ [line | y <- [0..bottom+1], let line = makeLine y]
  