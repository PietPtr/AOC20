
import Debug.Trace


rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

testinput = convert [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"]

type Forest = [[Tile]]
data Tile = Tree | Open deriving (Show, Eq)

char2tile :: Char -> Tile
char2tile '#' = Tree
char2tile '.' = Open

convert :: [String] -> Forest
convert forest = map convertrow forest
    where 
        convertrow row = map char2tile row


ride :: (Int, Int) -> Forest -> [Tile]
ride _ [] = []
ride (right, down) rows = [rows !! 0 !! 0] ++ ride (right, down) rows'
    where
        rows' = drop down $ map (rotate right) rows

count :: [Tile] -> Int
count = length . filter (== Tree)


main = (count . (ride (3, 1)) . convert . lines) <$> readFile "input"

-- 3.2

countAll :: Forest -> [Int]
countAll forest = map (\slope -> count $ ride slope forest) slopes
    where slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main2 = (foldl (*) 1 . countAll . convert . lines) <$> readFile "input"