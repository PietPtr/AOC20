import Data.List

testinput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"


data Instruction = Nop Int | Acc Int | Jmp Int
    deriving Show

type MarkedInstruction = (Bool, Instruction)

parse :: String -> [Instruction]
parse input = map parseLine textlines
    where
        parseLine (i:line) = case i of
            'n' -> Nop (extractValue line)
            'a' -> Acc (extractValue line)
            'j' -> Jmp (extractValue line)
        
        extractValue :: String -> Int
        extractValue subline = sign $ read $ snd $ splitAt 4 subline
            where sign = if (subline !! 3 == '-') then negate else id

        textlines = lines input

data State = State Int Int [MarkedInstruction]
    deriving Show

run :: State -> Int
run (State pc acc minstrs) = output
    where
        (already, instr) = minstrs !! pc

        output = if pc >= length minstrs
            then acc
            else run state'

        state' = State pc' acc' minstrs'

        minstrs' = first ++ [(True, instr)] ++ snd
            where (first, (_, instr):snd) = splitAt pc minstrs

        pc' = case instr of
            Jmp offset -> pc + offset
            _ -> pc + 1

        acc' = case instr of
            Acc value -> acc + value
            _ -> acc


terminates :: State -> Bool
terminates (State pc acc minstrs) = output
    where
        output = if pc >= length minstrs 
            then True
            else (if already then False else terminates state')


        (already, instr) = minstrs !! pc

        state' = State pc' acc' minstrs'

        minstrs' = first ++ [(True, instr)] ++ snd
            where (first, (_, instr):snd) = splitAt pc minstrs

        pc' = case instr of
            Jmp offset -> pc + offset
            _ -> pc + 1

        acc' = case instr of
            Acc value -> acc + value
            _ -> acc


altPrograms :: [Instruction] -> [[Instruction]]
altPrograms program = map makeAltProg (zip [0..] program)
    where
        -- progWithoutAcc = filter (\_ -> Tru) program
        -- isAcc instr = case instr of
        --     Acc _ -> True
        --     _ -> False

        makeAltProg (addr, (Jmp value) ) = replace addr program (Nop value)
        makeAltProg (addr, (Nop value) ) = replace addr program (Jmp value)

        makeAltProg (addr, instr) = replace addr program $ case instr of
            Nop value -> Jmp value
            Jmp value -> Nop value
            acc -> acc



replace idx list value = first ++ [value] ++ second
    where (first, _:second) = splitAt idx list

falses = map (\_ -> False) [1,1..]

solve input = out
    where
        out = run (State 0 0 (zip falses parsed))
        parsed = parse input

main = solve <$> readFile "input"

solve2 input = run (State 0 0 termingProg)
    where
        termingProg = zip falses $ programOptions !! termingIdx
        (Just termingIdx) = elemIndex True doProgramsTerm

        doProgramsTerm = 
            map (\prog -> terminates (State 0 0 (zip falses prog))) programOptions

        programOptions = altPrograms parsed

        parsed = parse input

main2 = solve2 <$> readFile "input"