
replace :: Char -> Char -> String -> String
replace a b = foldl (\str c-> str ++ (if c == a then [b] else [c])) ""

translate :: String -> String
translate = (replace 'F' '0') . (replace 'B' '1') . (replace 'L' '0') . (replace 'R' '1')

binToInt :: String -> Int
binToInt bin = foldl (\int num -> int * 2 + btoi num) 0 bin

btoi '0' = 0
btoi '1' = 1

main = ((foldl max 0) . (map (binToInt . translate)) . lines) <$> readFile "input"