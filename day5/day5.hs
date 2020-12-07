import Data.List

replace :: Char -> Char -> String -> String
replace a b = foldl (\str c-> str ++ (if c == a then [b] else [c])) ""

translate :: String -> String
translate = (replace 'F' '0') . (replace 'B' '1') . (replace 'L' '0') . (replace 'R' '1')

binToInt :: String -> Int
binToInt bin = foldl (\int num -> int * 2 + btoi num) 0 bin

btoi '0' = 0
btoi '1' = 1

allIds :: String -> [Int]
allIds = map (binToInt . translate) . lines

ordered :: [Int] -> [(Int, Int)]
ordered = zip [80..] . sort

inEqualTuple :: [(Int, Int)] -> Maybe (Int, Int)
inEqualTuple ts = foldl find Nothing ts
    where 
        find Nothing (a,b)
            | a /= b = Just (a,b)
            | otherwise = Nothing
        find (Just t) _ = Just t

main = ((foldl max 0) . allIds) <$> readFile "input"

main2 = ((\(Just (a,_)) -> a) . inEqualTuple . ordered . allIds) <$> readFile "input"