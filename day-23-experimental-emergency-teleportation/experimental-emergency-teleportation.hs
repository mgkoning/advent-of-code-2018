import Data.List (sortOn, foldl')
import Text.ParserCombinators.Parsec (Parser, sepBy, getInput, setInput, (<|>), sepEndBy, parse)
import Text.ParserCombinators.Parsec.Char (char, string)
import Numeric (readDec, readSigned)
import Control.Applicative (empty)

type Vector3 = (Int, Int, Int)

origin = (0, 0, 0)

data Nanobot = Nanobot { botId :: Int, pos :: Vector3, radius :: Int} deriving (Show, Eq, Ord)

data SearchCube = SearchCube { botCount :: Int, originDistance :: Int, size :: Int, minCorner :: Vector3 } deriving (Show, Eq, Ord)

betterCube (SearchCube botsA distanceA sizeA _) (SearchCube botsB distanceB sizeB _) =
  case compare botsA botsB of
    EQ -> case compare distanceA distanceB of
            EQ -> sizeA < sizeB
            LT -> True
            GT -> False
    LT -> False
    GT -> True

inRangeCount nanobots = length $ filter (inRange baddestBot) nanobots
  where baddestBot = head $ reverse $ sortOn radius nanobots
        inRange (Nanobot _ baddestPos radius) (Nanobot _ pos _) = distance baddestPos pos <= radius

inRange (Nanobot _ p1 radius) (Nanobot _ p2 _) = distance p1 p2 <= radius

distance (x, y, z) (x', y', z') = sum [abs $ x - x', abs $ y - y', abs $ z - z']

{- "Inspired" by several threads about this problem on the subreddit. I was stuck on this for so many
   hours I sort of gave up.
   Essentially, what happens here is we find the coordinate by iterated subdivision of the space into cubes
   until the 'best' cube so far has size 1. That cube is the winner.
   We start out with a cube big enough to touch all bots, and continually subdivide into 8 subcubes until we're done.
-}
part2 nanobots = let (SearchCube botCount originDistance _ pos, _) = findBestCoordinate [initialCube] in (originDistance, botCount, pos)
  where allBots = length nanobots 
        initialCube = head $ dropWhile ((<allBots) . botCount) $ map makeInitialCube $ [1..]
        makeInitialCube powerOfTwo = let size = 2^powerOfTwo
                                         halfSize = 2^(powerOfTwo-1)
                                     in buildCube nanobots size (-halfSize, -halfSize, -halfSize)
        findBestCoordinate (c:cubes)
          | (size c) == 1 = (c, cubes)
          | otherwise = findBestCoordinate newQueue
          where subCubes = split nanobots c
                newQueue = foldl' sortedInsert cubes subCubes
        sortedInsert list b = let (prefix, suffix) = break (betterCube b) list in prefix ++ (b:suffix)

split nanobots (SearchCube _ _ size minCorner) =
  map (buildCube nanobots halfSize) (newCorners minCorner halfSize)
  where halfSize = size `div` 2
        newCorners (x, y, z) halfSize = [(x', y', z') | x' <- [x, x + halfSize], y' <- [y, y + halfSize], z' <- [z, z + halfSize]]

buildCube nanobots size pos =
  let botCount = countBots nanobots pos (opposite pos size)
      originDistance = distance origin cubeCenter
      cubeCenter = let (x, y, z) = pos
                       halfSize = size `div` 2
                   in (x + halfSize, y + halfSize, z + halfSize)
  in SearchCube botCount originDistance size pos

countBots bots minCorner maxCorner = length $ filter (overlapsCube minCorner maxCorner) bots

overlapsCube (minX, minY, minZ) (maxX, maxY, maxZ) (Nanobot _ (botX, botY, botZ) radius) =
  (axisDistance botX minX maxX) + (axisDistance botY minY maxY) + (axisDistance botZ minZ maxZ) <= radius
  where axisDistance x min max = let max' = max - 1 in if x < min then min - x else if max' < x then x - max' else 0

opposite (x, y, z) size = (x + size, y + size, z + size)

solve = do
  nanobots <- parseNanobots <$> readFile "input2.txt"
  putStrLn "Part 1:"
  print $ inRangeCount nanobots
  putStrLn "Part 2:"
  let (originDistance, botCount, pos) = part2 nanobots
  putStrLn $ (show originDistance) ++ " (" ++ (show botCount) ++ " bots at " ++ (show pos) ++ ")"

parseNanobots input = zipWith toNanobot [0..] posRadius
  where toNanobot id (pos, radius) = Nanobot id pos radius
        posRadius = case parse (parsePosRadius `sepEndBy` eol) "" input of
                            Left msg -> error (show msg)
                            Right parsed -> parsed

eol :: Parser String
eol = string "\r\n" <|> string "\n"

parsePosRadius :: Parser (Vector3, Int)
parsePosRadius = (,) <$ string "pos=<" <*> parseVector3 <* string ">, r=" <*> parseInt

parseVector3 :: Parser Vector3
parseVector3 = mkVector3 <$> parseInt `sepBy` (char ',')
  where mkVector3 (a:b:c:[]) = (a,b,c)

parseInt :: Parser Int
parseInt = do s <- getInput
              case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

testNanobots@(n1:n2:n3:n4:n5:n6:[]) = parseNanobots $ unlines [
  "pos=<10,12,12>, r=2",
  "pos=<12,14,12>, r=2",
  "pos=<16,12,12>, r=4",
  "pos=<14,14,14>, r=6",
  "pos=<50,50,50>, r=200",
  "pos=<10,10,10>, r=5"]
