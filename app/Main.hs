import           Data.Function (fix)
import           System.IO     (hFlush, stdout)
import           Text.Read     (readEither)

data Solution a
    = NoSolutions
    | OneSolution a
    | TwoSolutions a a
    deriving (Show)

flush :: IO ()
flush = hFlush stdout

prompt :: String -> IO String
prompt text = do
    putStr text
    flush
    getLine

parsePrompt :: Read a => String -> IO a
parsePrompt text = fix $ \repeat -> do
    readEither <$> prompt text >>= \case
        Right value -> pure value
        Left  error -> do
            putStrLn error
            repeat

solveEquation :: (Floating a, Ord a) => a -> a -> a -> Solution a
solveEquation a b c
    | a == 0.0 = OneSolution alone
    | d <  0.0 = NoSolutions
    | d == 0.0 = OneSolution x1
    | d > 0.0  = TwoSolutions x1 x2
  where
    d     = b*b - 4*a*c
    x1    = f (+)
    x2    = f (-)
    alone = -c / b
    f op  = -b `op` sqrt d / (2*a)

main :: IO ()
main = do
    a <- parsePrompt "Input a: "
    b <- parsePrompt "Input b: "
    c <- parsePrompt "Input c: "
    print $ solveEquation a b c
