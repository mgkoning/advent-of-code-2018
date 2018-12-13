import Prelude hiding (Left, Right)
import Data.List (intercalate, sortOn, deleteBy, deleteFirstsBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Function (on)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

data MapElement = UpDown | LeftRight | UpLeftTurn | UpRightTurn | Intersection deriving (Show, Eq)

data Cart = Cart { cartID :: Integer, direction :: Direction, position :: (Integer, Integer), intersectionBehavior :: [(Direction -> Direction)] }
instance Show Cart where show c = intercalate " " $ ["Cart", (show $ cartID c), (show $ direction c), (show $ position c)]

data State = State { stateCarts :: [Cart], crashes :: [(Integer, Integer)], crashedCarts :: [Integer] } deriving (Show)

inputWithCoordinates :: String -> [((Integer, Integer), Char)]
inputWithCoordinates input = concatMap readLine $ zip [0..] $ lines input
  where readLine (y, line) = zip (zip [0..] (repeat y)) line

interpretInput :: String -> (Map (Integer, Integer) MapElement, [Cart])
interpretInput input = (railMap, carts)
  where indexedInput = filter ((/= ' ') . snd) $ inputWithCoordinates input
        railMap = toRailMap indexedInput
        carts = map toCart $ zip [0..] $ filter (isCart . snd) indexedInput
        isCart x = x `elem` "<>^v"
        toRailMap m = Map.map toMapElement $ Map.fromList m
        toMapElement c
          | c `elem` "|v^" = UpDown
          | c `elem` "-<>" = LeftRight
          | c == '\\' = UpLeftTurn
          | c == '/' = UpRightTurn
          | c == '+' = Intersection
        toCart (id, ((x, y), dir)) =
          case dir of 'v' -> makeCart Down; '<' -> makeCart Left; '>' -> makeCart Right; '^' -> makeCart Up
          where makeCart dir = Cart id dir (x, y) $ cycle [turnLeft, goStraight, turnRight]

turnLeft dir  = case dir of Up -> Left;  Left -> Down;  Down -> Right; Right -> Up
turnRight dir = case dir of Up -> Right; Right -> Down; Down -> Left;  Left -> Up
goStraight = id

tick :: Map (Integer, Integer) MapElement -> State -> State
tick railMap state = foldl (updateStateForCart railMap) state (sortCarts $ stateCarts state)

updateStateForCart railMap (State oldCarts oldCrashes oldCrashedCarts) cart
  | (cartID cart) `elem` oldCrashedCarts = (State (removeCart oldCarts cart) oldCrashes oldCrashedCarts)
  | otherwise = State newCarts (newCrashes) (newCrashedCarts)
  where newCarts = filter (\c -> (cartID c) `notElem` newCrashedCarts) (replaceCart oldCarts movedCart)
        replaceCart cs c = c:(removeCart cs c)
        removeCart cs c = deleteBy ((==) `on` cartID) c cs
        movedCart = move railMap cart
        newPosition = position movedCart
        collidingCarts = filter ((== newPosition) . position) oldCarts
        (newCrashes, newCrashedCarts) =
           if null collidingCarts
             then (oldCrashes, oldCrashedCarts)
             else (newPosition:oldCrashes, (map cartID collidingCarts) ++ ((cartID cart):oldCrashedCarts))

sortCarts = sortOn (swap . position)

move railMap (Cart id direction position behaviors) = Cart id newDirection newPosition newBehaviors
  where newPosition = add position (deltaFor direction)
        newMapElement = railMap ! newPosition
        (newDirection, newBehaviors) = determineTurn newMapElement direction behaviors

deltaFor Up    = ( 0,-1)
deltaFor Down  = ( 0, 1)
deltaFor Left  = (-1, 0)
deltaFor Right = ( 1, 0)

determineTurn Intersection direction behaviors = (head behaviors $ direction, drop 1 behaviors)
determineTurn LeftRight    direction b         = (direction, b)
determineTurn UpDown       direction b         = (direction, b)

determineTurn UpLeftTurn   Left      b         = (Up,    b)
determineTurn UpLeftTurn   Right     b         = (Down,  b)
determineTurn UpLeftTurn   Up        b         = (Left,  b)
determineTurn UpLeftTurn   Down      b         = (Right, b)

determineTurn UpRightTurn  Left      b         = (Down,  b)
determineTurn UpRightTurn  Right     b         = (Up,    b)
determineTurn UpRightTurn  Up        b         = (Right, b)
determineTurn UpRightTurn  Down      b         = (Left,  b)

add (x, y) (x', y') = (x, y) `seq` (x + x', y + y')

tickingAlong r c = iterate (tick r) (State c [] [])

part1 railMap carts = head $ reverse $ crashes $ head $ dropWhile (\s -> null $ crashedCarts s) $ tickingAlong railMap carts

part2 railMap carts = position $ head $ stateCarts $ head $ dropWhile (\s -> (length $ stateCarts s) > 1) $ tickingAlong railMap carts

solve = do
  (railMap, carts) <- interpretInput <$> readFile "input.txt"
  putStrLn "Part 1:"
  showResult $ part1 railMap carts
  putStrLn "Part 2:"
  showResult $ part2 railMap carts
    where showResult (x, y) = putStrLn ((show x) ++ "," ++ (show y))

testInput = 
  "/->-\\        \n" ++
  "|   |  /----\\\n" ++
  "| /-+--+-\\  |\n" ++
  "| | |  | v  |\n" ++
  "\\-+-/  \\-+--/\n" ++
  "  \\------/   \n" 

(testRailMap, testCarts) = interpretInput testInput