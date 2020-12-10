import Data.List
import Debug.Trace

testinput1 = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
testinput = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

parse :: String -> [Int]
parse = (map read) . lines

fix :: [Int] -> [Int]
fix chargers = 0 : sorted ++ [last sorted + 3]
    where sorted = sort chargers

solve :: String -> Int
solve input = fst counts * snd counts
    where
        counts = cnt (0, 0) sorted
        cnt result [a] = result
        cnt (ones, threes) (a:b:rest) = cnt count' (b:rest)
            where 
                count' = case b - a of
                    3 -> (ones, threes + 1)
                    1 -> (ones + 1, threes)
                    _ -> (ones, threes)

        sorted = fix $ parse input


main = solve <$> readFile "input"

diffs :: [Int] -> [Int]
diffs [b] = []
diffs (a:b:xs) = (b - a) : (diffs (b:xs))

{--
Notice that when calling diffs on the puzzle input, the largest string of consecutive differences
of 1's is four. I explored the possible combinations of all chains of 1's up to and including four,
yielding the following conversion:

Tree for four 1s:

   1111
 /   |   \
211 121 112
 \ X  X  /
 31 22 13

So, seven combinations.
--}

calc :: [Int] -> Int
calc [] = 1
calc (1:1:1:1:xs) = 7 * (calc xs)
calc (1:1:1:xs) = 4 * (calc xs)
calc (1:1:xs) = 2 * (calc xs)
calc (_:xs) = calc (xs)



solve2' input = (calc' . diffs . fix . parse) input

main2 = solve2' <$> readFile "input"

-- calculate the coefficient hardcoded in the calc function
coeff :: Int -> Int
coeff n = length $ unique $ reduceAll start
    where
        start = take n $ repeat 1

reduce :: [Int] -> Int -> [Int]
reduce xs idx = first ++ [a + b] ++ second
    where
        (first, a:b:second) = splitAt idx xs

reducable a b = (a == 1 && b == 1) || (a == 1 && b == 2) || (a == 2 && b == 1)

reducableIndices :: [(Int, Int)] -> [Int]
reducableIndices [(_, _)] = []
reducableIndices ( (v1, idx1):(v2, idx2):xs ) = index ++ (reducableIndices ((v2, idx2):xs))
    where 
        index = if reducable v1 v2
            then [idx1]
            else []

reduceAll :: [Int] -> [[Int]]
reduceAll xs = [xs] ++ (concat $ map reduceAll nextLists)
    where
        nextLists = map (\idx -> reduce xs idx) reducables 

        reducables = reducableIndices indexed
        indexed = zip xs [0..]

calc' :: [Int] -> Int
calc' [] = 1
calc' xs = coeff oneLength * calc rest
    where
        rest = snd $ splitAt (oneLength + 1) xs
        oneLength = length oneString 
        oneString = takeWhile (== 1) xs

-- brute-force solution, too slow

valid :: [Int] -> Bool
valid [_] = True
valid (a:b:rest) = (b - a) <= 3 && valid (b:rest)

remove :: [Int] -> Int -> [Int]
remove list idx = first ++ second
    where
        (first, _:second) = splitAt idx list

unique :: Eq a => [a] -> [a] 
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

removedValids :: [Int] -> [[Int]]
removedValids chargers =  nextValids ++ next
    where
        next = if nextValids == []
            then []
            else (concat $ map (removedValids) nextValids)
        nextValids = filter valid $ map removed [1..(length chargers - 2)]
        removed n = remove chargers n


findCombinations :: [Int] -> [[Int]]
findCombinations chargers = chargers : (removedValids chargers)


solve2 :: String -> [[Int]]
solve2 input = findCombinations (fix $ parse input)