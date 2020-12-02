
import Data.List
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding (count)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as Token

testinput = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]

languageDef =
    emptyDef { Token.reservedNames   = [] }

lexer = Token.makeTokenParser languageDef

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

data PW = PW Int Int Char String
    deriving (Show)

parseMin = integer <* (char '-') 

parseMax = integer <* spaces

parseChar = letter <* (char ':') <* spaces

parsePWReq = PW <$> parseMin <*> parseMax <*> parseChar <*> many letter

pwreqs :: [String] -> [PW]
pwreqs input = adts
    where 
        parseResult = map (parse parsePWReq "") input
        adts = foldl (\x y -> x ++ checkParse y) [] parseResult

        checkParse result = case result of
            Right adt -> [adt]
            Left _  -> error "parse failed!"

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

valid :: PW -> Bool
valid (PW min max c pwstr) = cnt >= min && cnt <= max
    where
        cnt = count c pwstr

countCorrect :: (PW -> Bool) -> [String] -> Int
countCorrect validator input  = (length . filter id) bools
    where
        bools = map validator $ pwreqs input


input :: IO [String]
input = lines <$> readFile "input"
        
main = (countCorrect valid) <$> input

-- day 2.1

valid' :: PW -> Bool
valid' (PW pos1 pos2 c pwstr) = (pwstr !! idx1 == c) /= (pwstr !! idx2 == c)
    where
        idx1 = pos1 - 1
        idx2 = pos2 - 1