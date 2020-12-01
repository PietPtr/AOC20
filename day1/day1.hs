
import Data.List


rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

sum3lists xs ys zs = zipWith (+) zs (zipWith (+) xs ys)

sumAllFor :: Int -> Int -> [Int] -> [Int]
sumAllFor fd sd xs = sum3lists xs xs' xs''
    where
        xs' = rotate fd xs
        xs'' = rotate (fd + sd) xs


allDisplacements :: [Int] -> [((Int, Int), [Int])]
allDisplacements xs = map (\(x, y) -> ((x, y), sumAllFor x y xs)) dispCombis
    where
        dispCombis = [(x, y) | x <- [1..maxind], y <- [1..maxind], x + y < maxind]
        maxind = length xs

-- find2020list :: [(Int, Int), [Int]] -> (Int, Int)
find2020list dsps = filter (\((_,_), sums) -> 2020 `elem` sums) dsps

calcIndex :: ((Int, Int), [Int]) -> Maybe (Int, Int, Int)
calcIndex ((x, y), listwith2020) = (\i -> (i, x + i, (y + i))) <$> elemIndex 2020 listwith2020

find2020sum' :: Int -> [Int] -> Maybe Int
find2020sum' _ [] = Nothing
find2020sum' search (x:xs) = case x + search of
    2020 -> Just x
    _ -> find2020sum' search xs

find2020sum :: [Int] -> (Int, Int)
find2020sum (x:xs) = case result of
    Nothing -> find2020sum xs
    Just y -> (x, y)
    where
        result = find2020sum' x xs

main2 = calcIndex <$> the2020list
    where
        the2020list = (head . find2020list . allDisplacements) <$> entries

        entries :: IO [Int]
        entries = (map read) <$> strlines
        strlines = lines <$> readFile "input"


main1 = multed
    where
        multed = (\(x, y) -> x * y) <$> summers

        summers = find2020sum <$> entries

        entries :: IO [Int]
        entries = (map read) <$> strlines
        strlines = lines <$> readFile "input"
