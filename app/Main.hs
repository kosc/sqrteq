import           System.IO (hFlush, stdout)
import           Text.Read (readMaybe)

data Solution a
    = NoSolutions
    | OneSolution a
    | TwoSolutions a a
    deriving (Show)

prompt :: Read a => String -> IO a
prompt text = do
    putStr text
    hFlush stdout
    readMaybe <$> getLine >>= \case
        Just x  -> pure x
        Nothing -> do
            putStrLn "Invalid number entered"
            prompt text

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
    a <- prompt "Input a: "
    b <- prompt "Input b: "
    c <- prompt "Input c: "
    print $ solveEquation a b c
