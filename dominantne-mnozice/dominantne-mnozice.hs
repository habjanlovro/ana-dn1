import System.IO
import Debug.Trace


inD :: Int -> Int -> [[(Int, Int)]]
inD n k =
  [[(i, x) | x <- [1..k]] | i <- [1..n]]


notTwoTimes :: Int -> Int -> [[(Int, Int)]]
notTwoTimes n k =
  [[(-i, -x), (-i, -y)] | i <- [1..n], x <- [1..k-1], y <- [x+1..k]]


validConnection :: Int -> Int -> Int -> [[(Int, Int)]]
validConnection k v u =
  [[(v, i), (u, j)] | i <- [1..k], j <- [1..k], i /= j]


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

readTillEnd :: Int -> [String] -> IO ([String])
readTillEnd iters input = do
  if iters == 0
    then return input
    else do
      l <- getLine
      readTillEnd (iters - 1) (l : input)

getEdgeClauses :: Int -> [String] -> [[(Int, Int)]]
getEdgeClauses k input =
  let edgeClause [] acc = acc
      edgeClause (l : ls) acc =
        let w = words l
            v = read (w !! 1) :: Int
            u = read (w !! 2) :: Int
            clauses = validConnection k v u
        in
          edgeClause ls (acc ++ clauses)
  in
    edgeClause input []

formatCall :: Int -> [[(Int, Int)]] -> String -> String
formatCall k [] acc = acc
formatCall k (c : cs) acc =
  let formatted = foldr
                  (\x a -> a ++ " " ++ show (calculateIndex k x))
                  (show (calculateIndex k (head c)))
                  (tail c)
        ++ " 0\n"
  in
    formatCall k cs (acc ++ formatted)

format :: [[(Int, Int)]] -> Int -> Int -> String
format clauses k n =
  let formattedClauses = formatCall k clauses ""
      numClauses = length clauses
      numVariables = n * k
  in
    ("p cnf " ++ show numVariables ++ " " ++ show numClauses ++ "\n" ++ formattedClauses)


main :: IO ()
main = do
--  putStrLn "Enter k: "
  k <- readLn :: IO Int
--  putStrLn "Vpiši graf:"
  fstLine <- getLine
  let (n, numEdges) = parseFirstLine fstLine
  edgesData <- readTillEnd numEdges []
  let edgeClauses = getEdgeClauses k edgesData
  let clauses = (inD n k) ++ (notTwoTimes n k) ++ edgeClauses
  let formatted = format clauses k n
  putStrLn formatted
