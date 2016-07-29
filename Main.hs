import System.IO
import Text.Read

data Solution a =
     OneSolution a
   | TwoSolutions a a
   | NoSolutions deriving (Show)

prompt :: (Read a) => String -> IO a
prompt text = do
    putStr text
    hFlush stdout
    line <- getLine
    case readMaybe line of
         Just x -> return x
         Nothing -> putStrLn "Invalid number entered" >> prompt text

solveEquation :: (Floating a, Ord a) => a -> a -> a -> Solution a
solveEquation a b c
    | a == 0.0 = OneSolution alone
    | d < 0.0 = NoSolutions
    | d == 0.0 = OneSolution x1
    | d > 0.0 = TwoSolutions x1 x2
    where d = b*b - 4*a*c
          x1 = f (+)
          x2 = f (-)
          f operator = -b `operator` sqrt d / (2*a)
          alone = -c / b

main :: IO ()
main = do
        a <- prompt "Input a: "
        b <- prompt "Input b: "
        c <- prompt "Input c: "
        print $ solveEquation a b c
