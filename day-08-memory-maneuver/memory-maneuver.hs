import qualified Data.Text as T
import Data.List (iterate')
import Data.Maybe (catMaybes)

data Node = Node { childNodes :: [Node], metadata :: [Int] } deriving Show

readLicense :: String -> [Int]
readLicense line = map (read . T.unpack) $ T.splitOn (T.pack " ") (T.pack line)

buildTree :: String -> Node
buildTree = fst . buildNode . readLicense

buildNode :: [Int] -> (Node, [Int])
buildNode (c:m:rest) = (thisNode, remaining)
  where thisNode = Node (reverse childNodes) metadata
        (childNodes, input) = readChildNodes c rest
        (metadata, remaining) = splitAt m input
        readChildNodes :: Int -> [Int] -> ([Node], [Int])
        readChildNodes n r = head $ drop n $ iterate' readChildNode ([], r)
        readChildNode (c, input) = let (n, rest) = buildNode input in (n:c, rest)

sumMetadata :: Node -> Int
sumMetadata node = sum (metadata node) + sum (map sumMetadata $ childNodes node)

nodeValue :: Node -> Int
nodeValue (Node children meta)
  | null children = sum meta
  | otherwise = sum $ catMaybes $ map (maybeNodeValue children) meta
  where maybeNodeValue n i = do 
          childNode <- tryGet n (i - 1)
          return $ nodeValue childNode

tryGet :: [a] -> Int -> Maybe a
tryGet xs index = if index < length xs || index < 0 then Just (xs !! index) else Nothing

solve = do
  node <- buildTree <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ sumMetadata node
  putStrLn "Part 2:"
  putStrLn $ show $ nodeValue node
