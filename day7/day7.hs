import Data.List
import Data.Char
import Text.Read

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as Token

import Debug.Trace


testinput = "\
\light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
\dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
\bright white bags contain 1 shiny gold bag.\n\
\muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
\shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
\dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
\vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
\faded blue bags contain no other bags.\n\
\dotted black bags contain no other bags."

testrule = Rule "light red" [(1,"bright white"),(2,"muted yellow")]

type Bag = String

data Rule = Rule Bag [(Int, Bag)]
    deriving (Show, Eq)

colorname :: Parser String
colorname = many letter

colormod :: Parser String
colormod = many letter

color :: Parser Bag
color = (\m n -> m ++ " " ++ n) <$> colormod <* char ' ' <*> colorname

bagdef :: Parser Bag
bagdef = color <* string " bag" <* optional (char 's')

bagcontent :: Parser (Int, Bag)
bagcontent = (\d b -> (read d, b)) <$> many digit <* char ' ' <*> bagdef 

nobags :: Parser [(Int, Bag)]
nobags = (\_ -> []) <$> string "no other bags"

rule :: Parser Rule
rule = Rule <$> bagdef <* string " contain " <*> (try nobags <|> bagrules) <* char '.'
    where 
        bagrules = (bagcontent `sepBy` (string ", "))

rules :: Parser [Rule]
rules = rule `sepBy` char '\n'

parseRules :: String -> [Rule]
parseRules input = case parsed of
    Right rules -> rules
    Left error -> []
    where
        parsed = parse rules "" input
        
getRuleForBag :: [Rule] -> Bag -> Rule
getRuleForBag allRules bag = (filter (\(Rule bag' _) -> bag' == bag) allRules) !! 0

canCarryGold :: [Rule] -> Rule -> Bool
canCarryGold allRules (Rule bag carryableBags) = if ("shiny gold" `elem` onlyBags) then True else 
    case carryableBags of
        [] -> False
        _ -> or $ map (canCarryGold allRules) nextRules
    where
        onlyBags = map snd carryableBags
        nextRules = map (getRuleForBag allRules) onlyBags

whichCanCarry :: [Rule] -> [Bag]
whichCanCarry allRules = map snd $ filter (fst) $
    zip (map (canCarryGold allRules) allRules) (map bagColor allRules)
    where bagColor (Rule color _) = color

main = (length . whichCanCarry . parseRules) <$> readFile "input"