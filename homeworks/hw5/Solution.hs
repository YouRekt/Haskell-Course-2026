module Solution () where
import Control.Monad.State
    ( MonadState(get), modify, evalState, execState, State )
import Data.Map (Map)
import qualified Data.Map as Map

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

-- Ex.2

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr

eval :: Expr -> State (Map String Int) Int
eval (Assign name expr) = do
    value <- eval expr
    modify (Map.insert name value)
    return value
eval (Var name) = do
    map <- get
    return $ map Map.! name
eval (Num n) = return n
eval (Add l r) = do
    l' <- eval l
    r' <- eval r
    return (l' + r')
eval (Mul l r) = do
    l' <- eval l
    r' <- eval r
    return (l' * r')
eval (Neg e) = do
    e' <- eval e
    return (-e')
eval (Seq l r) = eval l >> eval r

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- Ex.3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just dist -> return dist
        Nothing -> do
            dist <- compute
            modify (Map.insert (i, j) dist)
            return dist
    where
        compute
            | i == 0 = return j
            | j == 0 = return i
            | xs !! (i - 1) == ys !! (j - 1) =
                editDistM xs ys (i - 1) (j - 1)
            | otherwise = do
                del <- editDistM xs ys (i - 1) j
                ins <- editDistM xs ys i (j - 1)
                sub <- editDistM xs ys (i - 1) (j - 1)
                return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty