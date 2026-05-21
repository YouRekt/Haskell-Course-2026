module Solution () where

-- Ex.1

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader a) = Reader (f . a)

instance Applicative (Reader r) where
  pure x = Reader (const x)
  liftA2 f (Reader a) (Reader b) = Reader (\r -> f (a r) (b r))

instance Monad (Reader r) where
  Reader a >>= f = Reader (\r -> runReader (f (a r)) r)

-- Ex.2

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader a) = Reader (a . f)

-- Ex.3

data BankConfig = BankConfig
  { interestRate :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance :: Int
  } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    config <- ask
    return $ round $ fromIntegral (balance account) * interestRate config

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    config <- ask
    return $ account { balance = balance account - transactionFee config }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    config <- ask
    return $ balance account >= minimumBalance config

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    account' <- applyTransactionFee account
    interest <- calculateInterest account
    minimumBalance <- checkMinimumBalance account
    return (account', interest, minimumBalance)