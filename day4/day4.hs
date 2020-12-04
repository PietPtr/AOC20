import Data.List
-- import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as Token

sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x:xs)

replace :: String -> String
replace ('\n':'\n':xs) = '%' : '%' : replace xs
replace (x:xs)       = x : replace xs
replace ""           = ""


type Year = Int
type Height = Int
type Color = String
type ID = Integer

data Passport = Passport 
    { birthYear :: Maybe Year
    , issueYear :: Maybe Year
    , exprYear :: Maybe Year
    , height :: Maybe Height
    , hairColor :: Maybe Color
    , eyeColor :: Maybe Color
    , passportID :: Maybe ID
    , countryID :: Maybe ID
    } deriving (Show)

emptyPassport = Passport 
    { birthYear = Nothing
    , issueYear = Nothing
    , exprYear = Nothing
    , height = Nothing
    , hairColor = Nothing
    , eyeColor = Nothing
    , passportID = Nothing
    , countryID = Nothing }


languageDef =
    emptyDef { Token.reservedNames   = [] }

lexer = Token.makeTokenParser languageDef

symbol :: String -> Parser String
symbol = Token.symbol lexer

integer :: Parser Integer
integer = read <$> many digit

parseId :: Parser ID
parseId = integer

parseColor :: Parser Color
parseColor = ((:) <$> char '#' <*> many (letter <|> digit)) 
    <|> many letter

parseHeight :: Parser Height
parseHeight = read <$> many digit <* (symbol "cm" <|> symbol "in")

parseYear :: Parser Year
parseYear = read <$> count 4 digit

data PPField = 
    BirthYear Year | IssueYear Year | ExprYear Year |
    HeightField Height | EyeColor Color | HairColor Color | Pid ID | Cid ID
    deriving (Show)


parseYearField :: Parser PPField
parseYearField = 
    (decode <$> (symbol "byr" <|> symbol "iyr" <|> symbol "eyr")) 
    <* char ':' <*> parseYear
    where
        decode code = case code of
            "byr" -> BirthYear
            "iyr" -> IssueYear
            "eyr" -> ExprYear

parseHeightField :: Parser PPField
parseHeightField =
    (\_ -> HeightField) <$> symbol "hgt" <* char ':' <*> parseHeight

parseColorField :: Parser PPField
parseColorField =
    (decode <$> (symbol "ecl" <|> symbol "hcl")) <* char ':' <*> parseColor
    where
        decode code = case code of
            "ecl" -> EyeColor
            "hcl" -> HairColor

parseIdField :: Parser PPField
parseIdField = 
    (decode <$> (symbol "pid" <|> symbol "cid")) <* char ':' <*> parseId
    where
        decode code = case code of
            "pid" -> Pid
            "cid" -> Cid

parsePPElem :: Parser PPField
parsePPElem = try parseYearField 
    <|> try parseIdField 
    <|> try parseColorField 
    <|> try parseHeightField

parsePassport :: Parser Passport
parsePassport = buildPassport <$> parsePPElem `sepBy1` try (char ' ' <|> char '\n') <* symbol "%%"


parsePassports :: Parser [Passport]
parsePassports = many parsePassport

buildPassport :: [PPField] -> Passport
buildPassport fields = foldl addField emptyPassport fields

addField :: Passport -> PPField -> Passport
addField pp field = case field of
    BirthYear year -> if birthYear pp  == Nothing then pp { birthYear  = Just year } else error ""
    IssueYear year -> if issueYear pp  == Nothing then pp { issueYear  = Just year } else error ""
    ExprYear year  -> if exprYear pp   == Nothing then pp { exprYear   = Just year } else error ""
    HeightField h  -> if height pp     == Nothing then pp { height     = Just h    } else error ""
    EyeColor clr   -> if eyeColor pp   == Nothing then pp { eyeColor   = Just clr  } else error ""
    HairColor clr  -> if hairColor pp  == Nothing then pp { hairColor  = Just clr  } else error ""
    Pid id         -> if passportID pp == Nothing then pp { passportID = Just id   } else error ""
    Cid id         -> if countryID pp  == Nothing then pp { countryID  = Just id   } else error ""


-- workInput :: String -> [Passport]
workInput lines = parse parsePassports "" fixedstring
    where
        fixedstring = replace lines

main = workInput <$> readFile "input"


pp1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm%%"
pp2 = "ecl:gry hcl:z pid:860033327\n"
pp3 = "iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123%%"

testinput = 
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
    \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
    \\n\
    \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
    \hcl:#cfa07d byr:1929\n\
    \\n\
    \hcl:#ae17e1 iyr:2013\n\
    \eyr:2024\n\
    \ecl:brn pid:760753108 byr:1931\n\
    \hgt:179cm\n\
    \\n\
    \hcl:#cfa07d eyr:2025 pid:166559648\n\
    \iyr:2011 ecl:brn hgt:59in%%"