module Solution () where
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

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

-- Ex.4 Ex.5 Ex.6

data LocationType
    = Empty
    | Obstacle Int
    | Treasure Int
    | Trap Int
    | Goal
    deriving Show

data Location = Location
    { locName :: String
    , locType :: LocationType
    , locNext :: [Int]
    } deriving Show

data GameState = GameState
    { position :: Int
    , energy :: Int
    , score :: Int
    , board :: Map Int Location
    } deriving Show

type AdventureGame a = StateT GameState IO a

getDiceRoll :: IO Int
getDiceRoll = do
    putStr "Roll the die (1-6): "
    line <- getLine
    case readMaybe line of
        Just n | n >= 1 && n <= 6 -> return n
        _ -> putStrLn "Please enter a number between 1 and 6." >> getDiceRoll

getPlayerChoice :: [String] -> IO String
getPlayerChoice opts = do
    putStrLn "Choose a path:"
    mapM_ (\(i, o) -> putStrLn $ "  " ++ show i ++ ") " ++ o) (zip [1 :: Int ..] opts)
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just n | n >= 1 && n <= length opts -> return (opts !! (n - 1))
        _ -> putStrLn "Invalid choice, try again." >> getPlayerChoice opts

displayGameState :: GameState -> IO ()
displayGameState st = do
    putStrLn "========================================"
    let loc = Map.lookup (position st) (board st)
    putStrLn $ "Location : " ++ maybe "???" locName loc ++ " (#" ++ show (position st) ++ ")"
    putStrLn $ "Energy   : " ++ show (energy st)
    putStrLn $ "Score    : " ++ show (score st)
    putStrLn "========================================"

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    gameState <- get
    let walk 0 pos acc = (pos, acc)
        walk n pos acc = case Map.lookup pos (board gameState) of
            Nothing  -> (pos, acc)
            Just location -> case locType location of
                Goal -> (pos, acc)
                _ -> case locNext location of
                    [next] -> walk (n - 1) next (acc + 1)
                    _ -> (pos, acc)
    let (newPos, moved) = walk roll (position gameState) 0
    put gameState { position = newPos }
    liftIO $ putStrLn $ "You move " ++ show moved ++ " space(s)."
    return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    liftIO $ putStrLn "A choice lies before you..."
    liftIO $ getPlayerChoice options

handleLocation :: AdventureGame Bool
handleLocation = do
    gameState <- get
    case Map.lookup (position gameState) (board gameState) of
        Nothing -> return False
        Just location -> do
            liftIO $ putStrLn $ ">> " ++ locName location
            case locType location of
                Empty -> return ()
                Obstacle n -> do
                    liftIO $ putStrLn $ "An obstacle pushes you back " ++ show n ++ " space(s)!"
                    modify $ \s -> s { position = max 0 (position s - n) }
                Treasure n -> do
                    liftIO $ putStrLn $ "Treasure! +" ++ show n ++ " points."
                    modify $ \s -> s { score = score s + n }
                Trap n -> do
                    liftIO $ putStrLn $ "A trap! -" ++ show n ++ " points."
                    modify $ \s -> s { score = max 0 (score s - n) }
                Goal -> liftIO $ putStrLn "You have found the treasure!"
            case locType location of
                Goal -> return True
                _ -> case locNext location of
                    (_:_:_) -> do
                        let labels = [ maybe ("#" ++ show p) locName (Map.lookup p (board gameState)) | p <- locNext location ]
                        choice <- makeDecision labels
                        let nextPos = head [ p | (p, l) <- zip (locNext location) labels, l == choice ]
                        modify $ \s -> s { position = nextPos }
                        return False
                    _ -> return False

playTurn :: AdventureGame Bool
playTurn = do
    gameState <- get
    liftIO $ displayGameState gameState
    reached <- handleLocation
    if reached
        then return True
        else do
            gameState' <- get
            if energy gameState' <= 0
                then liftIO (putStrLn "You collapse from exhaustion!") >> return True
                else do
                    roll <- liftIO getDiceRoll
                    _ <- movePlayer roll
                    modify $ \s -> s { energy = energy s - 1 }
                    return False

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    if ended
        then do
            gameState <- get
            liftIO $ putStrLn "=== GAME OVER ==="
            liftIO $ displayGameState gameState
        else playGame