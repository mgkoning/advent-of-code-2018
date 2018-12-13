import Prelude hiding (Left, Right)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Direction = Up | Down | Left | Right deriving (Show, Eq)
data MapElement = UpDown | LeftRight | UpLeftTurn | UpRightTurn | Intersection deriving (Show, Eq)
data Cart = Cart { direction :: Direction, position :: (Integer, Integer), intersectionBehavior :: [(Direction -> Direction)] }
instance Show Cart where show c = intercalate " " $ ["Cart", (show $ direction c), (show $ position c)]

inputWithCoordinates :: String -> [((Integer, Integer), Char)]
inputWithCoordinates input = concatMap readLine $ zip [0..] $ lines input
  where readLine (y, line) = zip (zip [0..] (repeat y)) line

interpretInput input = (railMap, carts)
  where indexedInput = filter ((/= ' ') . snd) $ inputWithCoordinates input
        railMap = toRailMap indexedInput
        carts = map toCart $ filter (isCart . snd) indexedInput
        isCart x = x `elem` "<>^v"

toRailMap m = Map.map toMapElement $ Map.fromList m

toMapElement c
  | c `elem` "|v^" = UpDown
  | c `elem` "-<>" = LeftRight
  | c == '\\' = UpLeftTurn
  | c == '/' = UpRightTurn
  | c == '+' = Intersection

toCart ((x, y), dir) =
  case dir of
    'v' -> makeCart Down
    '<' -> makeCart Left
    '>' -> makeCart Right
    '^' -> makeCart Up
  where makeCart dir = Cart dir (x, y) $ cycle [turnLeft, goStraight, turnRight]

turnLeft dir  = case dir of Up -> Left;  Left -> Down;  Down -> Right; Right -> Up
turnRight dir = case dir of Up -> Right; Right -> Down; Down -> Left;  Left -> Up
goStraight = id


testInput = 
  "/->-\\        \n" ++
  "|   |  /----\\\n" ++
  "| /-+--+-\\  |\n" ++
  "| | |  | v  |\n" ++
  "\\-+-/  \\-+--/\n" ++
  "  \\------/   \n" 