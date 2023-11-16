module Main where

import Common (Expression (..))
import Evaluator (evaluate)

main :: IO ()
main = do
    -- Identity function applied to literal value 1
    let e1 = Application (Abstraction "x" (Variable "x")) (Literal 1)
    let e2 = Application (Literal 1) (Abstraction "x" (Variable "x"))
    let expression = e1
    let evaluatedExpression = evaluate expression
    putStrLn $ show expression ++ " → " ++ show evaluatedExpression