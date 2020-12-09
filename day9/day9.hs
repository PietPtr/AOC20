

testinput = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"

ps = 25 -- preamble size

hasSumPair :: [Int] -> Int -> Maybe (Int, Int)
hasSumPair xs sum = if length summableNums > 0
    then Just (firstNum, sum - firstNum)
    else Nothing
    where
        firstNum = summableNums !! 0
        summableNums = map fst $ filter snd pairs
        pairs = zip xs summableIdxs
        summableIdxs = map (\x -> (sum - x) `elem` (xs `without` x)) xs
        without list a = filter (/= a) list

verifyStart :: [Int] -> Maybe (Int, Int)
verifyStart numbers = hasSumPair numsForPair check
    where
        (numsForPair, check:_) = splitAt ps numbers

verifyList :: [Int] -> [Maybe (Int, Int)]
verifyList xs = verifyStart xs : rest
    where 
        rest = if length xs > (ps + 1)
            then verifyList $ tail xs
            else []
        
getInvalid :: [Int] -> [(Int, Maybe (Int, Int))]
getInvalid xs = filter ((== Nothing) . snd) $ zip noPreamble verified
    where  
        (_, noPreamble) = splitAt ps xs
        verified = verifyList xs

parse :: String -> [Int]
parse input = map read $ lines input

solve :: String -> Int
solve input = fst $ (getInvalid $ parse input) !! 0

main = solve <$> readFile "input"

invalid = 15353384

scanForSet :: [Int] -> Int -> Maybe [Int]
scanForSet [] _ = Nothing
scanForSet xs width = if doesSum 
    then Just search
    else scanForSet (tail xs) width
    where 
        doesSum = sum search == invalid
        (search, _) = splitAt width xs

crack :: [Int] -> Int
crack xs = maximum xs + minimum xs

solve2 input = crack $ foundSet 2
    where
        foundSet n = case scanForSet xs n of
            Just set -> set
            Nothing -> foundSet (n + 1)

        xs = parse input

main2 = solve2 <$> readFile "input"