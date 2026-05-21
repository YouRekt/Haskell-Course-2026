module Solution () where
import Control.Monad.State

-- Ex.1

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = modify $ \s -> case s of
    (_ : xs) -> xs
    _ -> s
execInstr DUP = modify $ \s -> case s of
    (x : xs) -> x : x : xs
    _ -> s
execInstr SWAP = modify $ \s -> case s of
    (x : y : xs) -> y : x : xs
    _ -> s
execInstr ADD = modify $ \s -> case s of
    (x : y : xs) -> (x + y) : xs
    _ -> s
execInstr MUL = modify $ \s -> case s of
    (x : y : xs) -> (x * y) : xs
    _ -> s
execInstr NEG = modify $ \s -> case s of
    (x : xs) -> (-x) : xs
    _ -> s

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []