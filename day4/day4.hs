import Data.List
import Data.Char
import Text.Read
-- import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace

sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x:xs)

replace :: String -> String
replace ('\n':'\n':xs) = '%' : '%' : replace xs
replace (x:xs)       = x : replace xs
replace ""           = ""


type Year = Int
data Height = CM Int | IN Int deriving (Show, Eq)
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

data Passport1 = Passport1
    { birthYear1 :: Bool
    , issueYear1 :: Bool
    , exprYear1 :: Bool
    , height1 :: Bool
    , hairColor1 :: Bool
    , eyeColor1 :: Bool
    , passportID1 :: Bool
    , countryID1 :: Bool
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


integer :: Parser Integer
integer = read <$> many digit

parseId :: Parser ID
parseId = integer

parseColor :: Parser Color
parseColor = ((:) <$> char '#' <*> many (letter <|> digit)) 
    <|> many lower
-- parseColor = many (letter <|> digit <|> char '#')

parseHeight :: Parser Height
parseHeight = converter <$> many digit <*> (string "cm" <|> string "in")
    where 
        converter digitstr measurement = case measurement of
            "cm" -> CM (read digitstr)
            "in" -> IN (read digitstr)


parseYear :: Parser Year
parseYear = read <$> count 4 digit

data PPField = 
    BirthYear Year | IssueYear Year | ExprYear Year |
    HeightField Height | EyeColor Color | HairColor Color | Pid ID | Cid ID
    deriving (Show)



parseYearField :: Parser PPField
parseYearField =
    (decode <$> (string "byr" <|> string "iyr" <|> string "eyr")) 
    <* char ':' <*> parseYear
    where
        decode code = case code of
            "byr" -> BirthYear
            "iyr" -> IssueYear
            "eyr" -> ExprYear

parseHeightField :: Parser PPField
parseHeightField =
    (\_ -> HeightField) <$> string "hgt" <* char ':' <*> parseHeight

parseColorField :: Parser PPField
parseColorField = 
    (decode <$> (string "ecl" <|> string "hcl")) <* char ':' <*> parseColor
    where
        decode code = case code of
            "ecl" -> EyeColor
            "hcl" -> HairColor

parseIdField :: Parser PPField
parseIdField = 
    (decode <$> (string "pid" <|> string "cid")) <* char ':' <*> parseId
    where
        decode code = case code of
            "pid" -> Pid
            "cid" -> Cid


parsePPElem :: Parser (Maybe PPField)
parsePPElem = do
    idres <- (optionMaybe parseIdField)
    colorres <- (optionMaybe parseColorField)
    yearres <- (optionMaybe parseYearField)
    heightres <- (optionMaybe parseHeightField)
    return $ case (yearres, idres, colorres, heightres) of
        (Just year, _, _, _) -> (Just year)
        (_, Just id, _, _)   -> (Just id)
        (_, _, Just color, _) -> (Just color)
        (_, _, _, Just height) -> (Just height)
        _ -> (Nothing)

-- ander plan: gebruik de parser van vraag 1, en doe op de saaie manier de validation i guesss...

parsePPElem' :: Parser (Maybe PPField)
parsePPElem' = (try $ optionMaybe parseYearField)
    <|> (try $ optionMaybe parseIdField)
    <|> (try $ optionMaybe parseColorField)
    <|> (try $ optionMaybe parseHeightField)

parsePassport :: Parser Passport
parsePassport = buildPassport <$> parsePPElem `sepBy1` try (char ' ' <|> char '\n') <* string "%%"

parsePassports :: Parser [Passport]
parsePassports = many parsePassport

buildPassport :: [Maybe PPField] -> Passport
buildPassport fields = foldl addField emptyPassport fields

addField :: Passport -> Maybe PPField -> Passport
addField pp (Just field) = case field of
    BirthYear year -> if birthYear pp  == Nothing then pp { birthYear  = Just year } else error ""
    IssueYear year -> if issueYear pp  == Nothing then pp { issueYear  = Just year } else error ""
    ExprYear year  -> if exprYear pp   == Nothing then pp { exprYear   = Just year } else error ""
    HeightField h  -> if height pp     == Nothing then pp { height     = Just h    } else error ""
    EyeColor clr   -> if eyeColor pp   == Nothing then pp { eyeColor   = Just clr  } else error ""
    HairColor clr  -> if hairColor pp  == Nothing then pp { hairColor  = Just clr  } else error ""
    Pid id         -> if passportID pp == Nothing then pp { passportID = Just id   } else error ""
    Cid id         -> if countryID pp  == Nothing then pp { countryID  = Just id   } else error ""
addField pp Nothing = pp


data PPField1 = 
    BirthYear1 | IssueYear1 | ExprYear1 |
    HeightField1 | EyeColor1 | HairColor1 | Pid1 | Cid1
    deriving (Show, Eq)

parsePPFieldPresence :: Parser PPField1
parsePPFieldPresence =
    decode <$> (
        string "pid" <|> string "cid" <|> 
        (try $ string "ecl") <|> (try $ string "hcl") <|> 
        string "byr" <|> string "iyr" <|>
        string "eyr" <|> string "hgt") <* char ':' <* (many (digit <|> letter <|> char '#'))
    where
        decode str = case str of
            "pid" -> Pid1
            "cid" -> Cid1
            "ecl" -> EyeColor1
            "hcl" -> HairColor1
            "byr" -> BirthYear1
            "iyr" -> IssueYear1
            "eyr" -> ExprYear1
            "hgt" -> HeightField1



parsePPField :: Parser (PPField1, String)
parsePPField = 
    (\fieldid content -> (decode fieldid, content)) <$> (
        string "pid" <|> string "cid" <|> 
        (try $ string "ecl") <|> (try $ string "hcl") <|> 
        string "byr" <|> string "iyr" <|>
        string "eyr" <|> string "hgt") <* char ':' <*> (many (digit <|> letter <|> char '#'))
    where
        decode str = case str of
            "pid" -> Pid1
            "cid" -> Cid1
            "ecl" -> EyeColor1
            "hcl" -> HairColor1
            "byr" -> BirthYear1
            "iyr" -> IssueYear1
            "eyr" -> ExprYear1
            "hgt" -> HeightField1



parsePassportFields :: Parser [(PPField1, String)]
parsePassportFields = 
    parsePPField `sepBy1` try (char ' ' <|> char '\n') <* string "%%"

-- validatePassports :: Parser [Bool]
-- validatePassports = (map validPassportFieldList) <$> many parsePassportFields

(∈) :: Eq a => a -> [a] -> Bool
(∈) = elem

validPassportFieldList :: [PPField1] -> Bool
validPassportFieldList fields = and
    [ Pid1 ∈ fields, EyeColor1 ∈ fields, HairColor1 ∈ fields, BirthYear1 ∈ fields, 
      IssueYear1 ∈ fields, ExprYear1 ∈ fields, HeightField1 ∈ fields ]

validPassportFieldList2 :: [(PPField1, String)] -> Bool
validPassportFieldList2 fieldsWithContent = validPassportFieldList fields
    where
        fields = map (\(f, c) -> f) fieldsWithContent


-- workInput :: String -> [Passport]
-- workInput lines = (length . filter id) <$> parse validatePassports "" fixedstring
--     where
--         fixedstring = replace lines


forall str p = (filter p str) == str

validateField' (field, content) = if (result && field == IssueYear1) then (trace (show input) result) else result
    where
        result = validateField input
        input = (field, content)

validateField :: (PPField1, String) -> Bool
validateField (BirthYear1, content) = 
    (length content) == 4 && forall content isDigit &&
    read content >= 1920 && read content <= 2002
validateField (IssueYear1, content) =
    (length content) == 4 && forall content isDigit &&
    read content >= 2010 && read content <= 2020
validateField (ExprYear1, content) =
    (length content) == 4 && forall content isDigit &&
    read content >= 2020 && read content <= 2030
validateField (HeightField1, content) = 
    unit ∈ ["cm", "in"] && validnumber &&
    (unit == "cm") <= (value >= 150 && value <= 193) &&
    (unit == "in") <= (value >= 59 && value <= 76)
    where 
        unit = (filter (not . isDigit) content)
        readValue = readMaybe (filter isDigit content)
        (validnumber, value) = case readValue of
            Just v -> (True, v)
            Nothing -> (False, 0)
validateField (HairColor1, (first:color)) =
    first == '#' && length color == 6 &&
    forall color (\c -> c `elem` ['0'..'9'] || c `elem` ['a'..'f'])
validateField (EyeColor1, content) =
    content ∈ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateField (Pid1, content) =
    length content == 9 && forall content isDigit
validateField (Cid1, content) = True


main2 = validated
    where
        parser = many parsePassportFields
        passports = ((parse parser "") . replace) <$> readFile "input"

        -- validated = (\(Right pps) -> (length . filter id) $ (map (and . map validateField') pps) ) <$> passports

        validated = (\(Right pps) -> (length . filter id) $ map isValidPassport pps) <$> passports

        isValidPassport pp = and validFields && validPassportFieldList2 pp
            where validFields = map validateField pp

        -- validateField = (\(Right pps) -> map (and . map validateField) pps ) <$> passports passports

-- main2 = (finalize . replace) <$> readFile "input"
--     where
--         finalize input = parse parsePassports "" input





pp1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm%%"
pp2 = "ecl:gry hcl:z pid:860033327\n"
pp3 = "iyr:2010 ecl:gry pid:591597745 hgt:181cm byr:1920 hcl:#6b5442 eyr:2029 cid:123%%"
pp4 = "byr:1970\necl:oth\neyr:2025\npid:409994798 iyr:2018 hgt:189cm%%"


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