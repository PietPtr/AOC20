
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