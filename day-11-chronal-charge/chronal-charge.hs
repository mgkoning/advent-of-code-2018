import Data.Map.Strict (fromList, (!), mapAccumWithKey, insert, findWithDefault)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

chargeAt serialNumber (x, y) = totalPower
  where rackID = x + 10
        initialPower = ((rackID * y) + serialNumber) * rackID
        hundredsDigit = initialPower `div` 100 `mod` 10
        totalPower = hundredsDigit - 5

fuelCellGrid serialNumber size = fromList [((x,y), power) | x <- [1..size], y <- [1..size], let power = chargeAt serialNumber (x,y)]

maxPower size sat squareSize = maximumBy (comparing snd) $ map (powerAt sat squareSize) [(x, y) | x <- [1..size-(squareSize-1)], y <- [1..size-(squareSize-1)]]

powerAt sat size (x, y) = ((x, y), valAt (x' + size, y' + size) + valAt (x', y') - valAt (x' + size, y') - valAt (x', y' + size))
  where valAt (x, y) = findWithDefault 0 (x, y) sat
        (x', y') = (x-1, y-1) 

summedAreaTable grid = fst $ mapAccumWithKey sumAt (fromList []) grid 
  where sumAt table (x, y) val =
          let valueAt (x, y) = findWithDefault 0 (x, y) table
              sum = val + valueAt (x, y-1) + valueAt (x-1, y) - valueAt (x-1, y-1)
          in (insert (x, y) sum table, val)

solve = do
  let grid = fuelCellGrid 1133 300
  let sat = summedAreaTable grid
  
  putStrLn "Part 1:"
  let ((maxAtX, maxAtY), max) = maxPower 300 sat 3
  putStrLn $ (show maxAtX) ++ "," ++ (show maxAtY)

  putStrLn "Part 2:"
  let (size, ((maxAtX, maxAtY), max)) = maximumBy (comparing $ snd . snd) $ zip [1..300] $ map (maxPower 300 sat) [1..300]
  putStrLn $ (show maxAtX) ++ "," ++ (show maxAtY) ++ "," ++ (show size)

