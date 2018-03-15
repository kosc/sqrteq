import           Data.Monoid     ((<>))
import           System.IO.Extra (parsePrompt)

data Solution a
    = NoSolutions
    | OneSolution a
    | TwoSolutions a a

instance Show a => Show (Solution a) where
    show NoSolutions          = "There are no solutions"
    show (OneSolution x)      = "There is only one solution: " <> show x
    show (TwoSolutions x1 x2) = "Solutions: " <> show x1 <> ", " <> show 2

solveEquation :: (Floating a, Ord a) => a -> a -> a -> Solution a
solveEquation 0 b c = OneSolution (-c / b)
solveEquation a b c = case compare discriminant 0 of
    LT -> NoSolutions
    EQ -> OneSolution x1
    GT -> TwoSolutions x1 x2
  where
    discriminant = (b * b) - (4 * a * c)
    center       = -b / (2 * a)
    offset       = sqrt discriminant / (2 * a)
    x1           = center + offset
    x2           = center - offset
    alone        = -c / b

main :: IO ()
main = do
    a <- parsePrompt "Input a: "
    b <- parsePrompt "Input b: "
    c <- parsePrompt "Input c: "
    print $ solveEquation a b c
