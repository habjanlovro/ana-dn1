import System.Environment
import Debug.Trace



createOneEntry isOver isEnd increment root i acc =
  let item = [-root, -i]
  in
    if isOver i then
      reverse acc
    else if isEnd i then
      reverse (item : acc)
    else
      createOneEntry isOver isEnd increment root (increment i) (item : acc)

createLines n startLine  =
  let endLine = startLine + n - 2
      indexes = [startLine .. endLine]
      lines = map (\i -> createOneEntry (\x -> n < x) (\x -> n == x) (\i -> i + 1) i (i + 1) []) indexes
  in
    concat lines

createColumns n startColumn =
  let indexes = map (\i -> n * (i-1) + startColumn) [1 .. n - 1]
      endColumn = (n-1) * n + startColumn
      columns = map (\i -> createOneEntry (\x -> endColumn < x) (\x -> endColumn == x) (\x -> x + n) i (i + n) []) indexes
  in
    concat columns


createDiagonalOne n startIndex =
  let indexes = map (\i -> startIndex + (i - 1) * (n + 1)) [1 .. (n - (startIndex `mod` n))]
      diagonals = map (\i -> createOneEntry (\x -> x > n * n) (\x -> x `mod` n == 0) (\x -> x + n + 1) i (i + n + 1) []) indexes
  in
    concat diagonals

createDiagonalTwo n startIndex =
  let indexes = map (\i -> startIndex + (i - 1) * (n - 1)) [1 .. (startIndex - 1) `mod` n]
      diagonals = map (\i -> createOneEntry (\x -> x > n * n) (\x -> x `mod` n == 1) (\x -> x + n - 1) i (i + n - 1) []) indexes
  in
    concat diagonals

atLeastOneClause :: Int -> [[Int]]
atLeastOneClause n =
  let lines = map (\x -> [(x-1)*n+1 .. x*n]) [1..n]
      columns = map (\x -> map (\i -> n * (i-1) + x) [1 .. n]) [1 .. n]
  in
    lines ++ columns

createClauses :: Int -> [[Int]]
createClauses n =
  let lineIndexes = (map (\x -> 1 + n * (x - 1)) [1 .. n])
      columnIndexes = [1 .. n]
      diagonalOneIndexes = columnIndexes ++ (tail lineIndexes)
      diagonalTwoIndexes = columnIndexes ++ (tail (map (\x -> x + n - 1) lineIndexes))
      allLines = map (\x -> createLines n x) lineIndexes
      allColumns = map (\x -> createColumns n x) columnIndexes
      allOneDiagonals = map (\x -> createDiagonalOne n x) diagonalOneIndexes
      allTwoDiagonals = map (\x -> createDiagonalTwo n x) diagonalTwoIndexes
  in
    atLeastOneClause n ++ concat (allLines ++ allColumns ++ allOneDiagonals ++ allTwoDiagonals)


toString clauses n =
  let formattedClauses = formatClauses clauses ""
      firstLine = "p cnf " ++ show (n * n) ++ " " ++ show (length clauses) ++ "\n"
  in
    firstLine ++ formattedClauses

formatClauses [] str = str
formatClauses (clause : clauses) str =
  formatClauses clauses (str ++ formatOne clause ++ "\n")

formatOne :: [Int] -> [Char]
formatOne (x : xs) =
  foldr (\x -> \acc -> acc ++ " " ++ show x) (show x) xs ++ " 0"


main :: IO ()
main = do
  args <- getArgs
  if length args > 0 then do
    let n = read (head args) :: Int
    let clauses = createClauses n
    let formatted = toString clauses n
    putStrLn formatted
    else
    putStrLn "Give size of the chessboard as command line argument!"
