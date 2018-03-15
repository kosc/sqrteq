import           Control.Applicative    (liftA3)

import           System.IO.Extra        (parsePrompt)

import           Data.QuadraticEquation (QuadraticEquation (..))
import qualified Data.QuadraticEquation as QuadraticEquation

main :: IO ()
main = do
    equation <- liftA3 QuadraticEquation
        (parsePrompt "Input a: ")
        (parsePrompt "Input b: ")
        (parsePrompt "Input c: ")
    print (QuadraticEquation.solve equation)
