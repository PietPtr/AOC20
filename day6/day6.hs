
testinput = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"

unique :: String -> String 
unique "" = ""
unique (x:xs) = x : unique (filter (/= x) xs)

makeGroups :: [String] -> [String]
makeGroups textInput = filter (/= "") $ foldl folder [] textInput
    where
        folder [] answer = [answer]
        folder groups "" = "" : groups
        folder (group:grps) answers = (group ++ answers) : grps

settify :: [String] -> [String]
settify groups = map unique groups

answer :: String -> Int
answer = sum . (map length) . settify . makeGroups . lines

main = answer <$> readFile "input"


makeGroups' :: [String] -> [[String]]
makeGroups' textInput = foldl folder [] textInput
    where
        folder [] answer = [[answer]]
        folder groups "" = [] : groups
        folder (group:grps) answer = (answer : group) : grps


everyone :: [String] -> String
everyone answers = map (\(c, _) -> c) onlyCorrect
    where 
        onlyCorrect = filter (\(_, b) -> b) corrects
        corrects = map (\a -> (a, predicate a)) ['a'..'z']
        predicate c = forall answers (\answer -> c `elem` answer)

answer2 :: String -> Int
answer2 = sum . (map length) . (map everyone) . makeGroups' . lines

main2 = answer2 <$> readFile "input"

forall str p = (filter p str) == str