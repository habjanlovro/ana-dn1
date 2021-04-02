import System.IO
import Debug.Trace
import Data.Graph
import Data.Array

everyVertexInD n k =
  [[(i, j) | i <- [1..n]] | j <- [1..k]]


notTwoTimes :: Int -> Int -> [[(Int, Int)]]
notTwoTimes n k =
  let sameVertexTwice = 
        [[(-i, -x), (-i, -y)] | i <- [1..n], x <- [1..k-1], y <- [x+1..k]]
      sameIndexTwice =
        [[(-x, -i), (-y, -i)] | i <- [1..k], x <- [1..n-1], y <- [x+1..n]]
  in
    sameVertexTwice ++ sameIndexTwice


getEdgesClauseForVertex :: Int -> [Int] -> Int -> [(Int, Int)]
getEdgesClauseForVertex index neighbors k =
  concat $ map (\x -> [(x, j) | j <- [1..k]]) (index : neighbors)
  

getEdges :: Graph -> Int -> [[(Int, Int)]]
getEdges g k =
  map (\(i, neighbors) -> getEdgesClauseForVertex i neighbors k) (assocs g)


calculateIndex :: Int -> (Int, Int) -> Int
calculateIndex k (i, j) =
  let isNegative = i < 0
      absI = abs i
      absJ = abs j
      index = k * (absI - 1) + absJ
  in
    if isNegative then - index else index


parseFirstLine :: String -> (Int, Int)
parseFirstLine s =
  let splitted = words s
      vertices = read (splitted !! 2) :: Int
      edges = read (splitted !! 3) :: Int
  in
    (vertices, edges)

readTillEnd :: Int -> IO ([String])
readTillEnd iters =
  let call i acc = do
        if i == 0
          then return acc
          else do
          l <- getLine
          call (i - 1) (l : acc)
  in
    call iters []

parseEdges :: [String] -> [(Int, Int)]
parseEdges input =
  let edgeClause [] acc = acc
      edgeClause (l : ls) acc =
        let w = words l
            v = read (w !! 1) :: Int
            u = read (w !! 2) :: Int
            e1 = (v, u)
            e2 = (u, v)
        in
          edgeClause ls (e1 : e2 : acc)
  in
    edgeClause input []

formatCall :: Int -> [[(Int, Int)]] -> String -> String
formatCall k [] acc = acc
formatCall k (c : cs) acc =
  let formatted = foldr
                  (\x a -> show (calculateIndex k x) ++ " " ++ a)
                  (show (calculateIndex k (head c)))
                  (tail c)
        ++ " 0\n"
  in
    formatCall k cs (formatted ++ acc)

format :: [[(Int, Int)]] -> Int -> Int -> String
format clauses n k =
  let formattedClauses = formatCall k clauses ""
      numClauses = length clauses
      numVariables = n * k
  in
    ("p cnf " ++ show numVariables ++ " " ++ show numClauses ++ "\n" ++ formattedClauses)


main :: IO ()
main = do
  fstLine <- getLine
  let (n, numEdges) = parseFirstLine fstLine
  edgesData <- readTillEnd numEdges
  k <- readLn :: IO Int
  let edges = parseEdges edgesData
  let graph = buildG (1, n) edges
  putStr $ format (everyVertexInD n k) n k
  putStr $ format (getEdges graph k) n k
  putStr $ format (notTwoTimes n k) n k
