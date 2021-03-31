import System.Environment
import Debug.Trace


createOneEntry isOver isEnd increment root =
  let call i acc =
        let item = [-root, -i]
        in
          if isOver i then
            reverse acc
          else if isEnd i then
            reverse (item : acc)
          else
            call (increment i) (item : acc)
  in
    call (increment root) []


createLines n startLine  =
  let endLine = startLine + n - 1
      indexes = [startLine .. endLine - 1]
      increment x = x + 1
      lines = [createOneEntry (\x -> endLine < x) (\x -> endLine == x) increment i | i <- indexes]
  in
    concat lines


createColumns n startColumn =
  let indexes = [n * (i - 1) + startColumn | i <- [1 .. n - 1]]
      endColumn = (n-1) * n + startColumn
      increment x = x + n
      columns = [createOneEntry (\x -> endColumn < x) (\x -> endColumn == x) increment i | i <- indexes]
  in
    concat columns


createDiagonalOne n startIndex =
  let indexes = [startIndex + (i - 1) * (n + 1) | i <- [1 .. (n - (startIndex `mod` n))]]
      increment x = x + n + 1
      diagonals = [createOneEntry (\x -> x > n * n) (\x -> x `mod` n == 0) increment i | i <- indexes]
  in
    concat diagonals

createDiagonalTwo n startIndex =
  let indexes = map (\i -> startIndex + (i - 1) * (n - 1)) [1 .. (startIndex - 1) `mod` n]
      increment x = x + n - 1
      diagonals = [createOneEntry (\x -> x > n * n) (\x -> x `mod` n == 1)  increment i | i <- indexes]
  in
    concat diagonals

atLeastOneClause :: Int -> [[Int]]
atLeastOneClause n =
  let lines = [[(x - 1) * n + 1 .. x * n] | x <- [1 .. n]]
      columns = [map (\i -> n * (i - 1) + x) [1 .. n] | x <- [1 .. n]]
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
