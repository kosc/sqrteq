import System.IO
import Text.Read

data Solution = 
     OneSolution Double
   | TwoSoulutions Double Double
   | NoSolutions deriving (Show)

promptDouble :: Read a => String -> IO a
promptDouble text = do
    putStr text
    hFlush stdout
    line <- getLine
    case readMaybe line of
         Just x -> return x
         Nothing -> putStrLn "Invalid number entered" >> promptDouble text

sqrtEquation :: Double -> Double -> Double -> Solution
sqrtEquation a b c
    | d < 0.0 = NoSolutions
    | d == 0.0 = OneSolution x1
    | d > 0.0 = TwoSoulutions x1 x2
    where d = b*b - 4*a*c
          x1 = f (+)
          x2 = f (-)
          f operator = operator (-b) (sqrt d)/2*a

main :: IO ()
main = do
        a <- promptDouble "Input a: "
        b <- promptDouble "Input b: "
        c <- promptDouble "Input c: "
        let result = show (sqrtEquation a b c)
        putStrLn result
