import Data.Map.Strict (fromList, toList, adjust)
import Data.Tuple (swap)
import Data.List (groupBy, sortBy, sort, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)

-- position=< 53242, -42275> velocity=<-5,  4>
readStar :: Int -> String -> ((Int, Int), (Int, Int))
readStar posSize line = ((read xPos, read yPos), (read xVel, read yVel))
  where (_, a) = splitAt 10 line
        (xPos, b) = splitAt posSize a
        (_, c) = splitAt 1 b
        (yPos, d) = splitAt (1+posSize) c
        (_, e) = splitAt 12 d
        (xVel, f) = splitAt 2 e
        (_, g) = splitAt 1 f
        yVel = take 3 g

readStarChart input = map (readStar 6) $ lines input
readTestStarChart input = map (readStar 2) $ lines input

moveStars n starChart = map moveStar starChart
  where moveStar ((x,y), vel) =
          let (vx, vy) = vel
              newPosX = x+(n*vx)
              newPosY = y+(n*vy)
          in newPosX `seq` newPosY `seq` ((newPosX, newPosY), vel)

starbox starState = (left, right, bottom, top) 
  where (left, right, bottom, top) = (minimum xs, maximum xs, minimum ys, maximum ys)
        positions = map fst starState
        (xs, ys) = unzip positions

size (left, right, bottom, top) = (abs (right - left)) * (abs (top - bottom))

paintStars :: [((Int, Int), (Int, Int))] -> String
paintStars starState = picture
  where (left, right, bottom, top) = starbox starState
        positions = map fst starState
        canvas = fromList [((x, y), '.') | x <- [(left-1)..(right+1)], y <- [(bottom-1)..(top+1)]]
        strokes = toList $ foldl (flip (adjust (const '#'))) canvas positions
        byLines = groupBy ((==) `on` (snd . fst)) $ sortBy (comparing (snd . fst)) $ strokes
        picture = unlines $ map ((map snd) . (sortBy (comparing (fst . fst)))) byLines

solve = do
  putStrLn "Part 1:"
  starChart <- readStarChart <$> readFile "input.txt"
  let smallest = fst $ minimumBy (comparing snd) $ zip [0..] $ map (size . starbox) $ take 1000 $ iterate (moveStars 1) $ moveStars 10000 starChart
  let target = 10000 + smallest
  let painting = (paintStars (moveStars target starChart))
  --writeFile ("stars@" ++ (show target) ++ ".txt") (paintStars (moveStars target starChart))
  putStrLn $ painting
  putStrLn "Part 2:"
  putStrLn $ show target


{- Test stuff -}

testMessageTime = moveStars 3 (readTestStarChart testInput)

testInput = "position=< 9,  1> velocity=< 0,  2>\n" ++
  "position=< 7,  0> velocity=<-1,  0>\n" ++
  "position=< 3, -2> velocity=<-1,  1>\n" ++
  "position=< 6, 10> velocity=<-2, -1>\n" ++
  "position=< 2, -4> velocity=< 2,  2>\n" ++
  "position=<-6, 10> velocity=< 2, -2>\n" ++
  "position=< 1,  8> velocity=< 1, -1>\n" ++
  "position=< 1,  7> velocity=< 1,  0>\n" ++
  "position=<-3, 11> velocity=< 1, -2>\n" ++
  "position=< 7,  6> velocity=<-1, -1>\n" ++
  "position=<-2,  3> velocity=< 1,  0>\n" ++
  "position=<-4,  3> velocity=< 2,  0>\n" ++
  "position=<10, -3> velocity=<-1,  1>\n" ++
  "position=< 5, 11> velocity=< 1, -2>\n" ++
  "position=< 4,  7> velocity=< 0, -1>\n" ++
  "position=< 8, -2> velocity=< 0,  1>\n" ++
  "position=<15,  0> velocity=<-2,  0>\n" ++
  "position=< 1,  6> velocity=< 1,  0>\n" ++
  "position=< 8,  9> velocity=< 0, -1>\n" ++
  "position=< 3,  3> velocity=<-1,  1>\n" ++
  "position=< 0,  5> velocity=< 0, -1>\n" ++
  "position=<-2,  2> velocity=< 2,  0>\n" ++
  "position=< 5, -2> velocity=< 1,  2>\n" ++
  "position=< 1,  4> velocity=< 2,  1>\n" ++
  "position=<-2,  7> velocity=< 2, -2>\n" ++
  "position=< 3,  6> velocity=<-1, -1>\n" ++
  "position=< 5,  0> velocity=< 1,  0>\n" ++
  "position=<-6,  0> velocity=< 2,  0>\n" ++
  "position=< 5,  9> velocity=< 1, -2>\n" ++
  "position=<14,  7> velocity=<-2,  0>\n" ++
  "position=<-3,  6> velocity=< 2, -1>\n"