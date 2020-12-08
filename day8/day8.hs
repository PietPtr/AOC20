

testinput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"


data Instruction = Nop | Acc Int | Jmp Int
    deriving Show

type MarkedInstruction = (Bool, Instruction)

parse :: String -> [Instruction]
parse input = map parseLine textlines
    where
        parseLine (i:line) = case i of
            'n' -> Nop
            'a' -> Acc (extractValue line)
            'j' -> Jmp (extractValue line)
        
        extractValue :: String -> Int
        extractValue subline = sign $ read $ snd $ splitAt 4 subline
            where sign = if (subline !! 3 == '-') then negate else id

        textlines = lines input

data State = State Int Int [MarkedInstruction]
    deriving Show


run (State pc acc minstrs) = output
    where
        (already, instr) = minstrs !! pc

        output = if already
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




falses = map (\_ -> False) [1,1..]

solve input = out
    where
        out = run (State 0 0 (zip falses parsed))
        parsed = parse input

main = solve <$> readFile "input"