import Data.Map.Strict (fromList, (!))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

chargeAt serialNumber (x, y) = totalPower
  where rackID = x + 10
        initialPower = ((rackID * y) + serialNumber) * rackID
        hundredsDigit = initialPower `div` 100 `mod` 10
        totalPower = hundredsDigit - 5

fuelCellGrid serialNumber size = fromList [((x,y), power) | x <- [1..size], y <- [1..size], let power = chargeAt serialNumber (x,y)]

maxPower size grid squareSize = maximumBy (comparing snd) $ map (totalPowerAt grid squareSize) [(x, y) | x <- [1..size-(squareSize-1)], y <- [1..size-(squareSize-1)]]

totalPowerAt grid squareSize (x, y) = ((x, y), powerSum)
  where powerSum = sum $ map (grid !) $ [(x+x', y+y') | x' <- [0..squareSize-1], y' <- [0..squareSize-1]]

solve = do
  let grid = fuelCellGrid 1133 300
  putStrLn "Part 1:"
  let (maxAt, max) = maxPower 300 grid 3
  putStrLn $ show $ maxAt

  putStrLn "Part 2:" {- just brute force it, i gots plenty time (though I put in a trace just in case) -}
  let (size, ((maxAtX, maxAtY), max)) = maximumBy (comparing $ snd . snd) $ map (\n -> trace (show n) n) $ zip [1..300] $ map (maxPower 300 grid) [1..300]
  putStrLn $ (show maxAtX) ++ "," ++ (show maxAtY) ++ "," ++ (show size)