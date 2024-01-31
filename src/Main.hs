module Main where

import Control.Monad (forever)
import Data.Map.Ordered qualified as Map
import Implementation.Evaluator (evaluate)
import System.IO (hFlush, stdout)
import Implementation.Terms

-- repl :: IO ()
-- repl = forever $ do
--     putStr "λ> "
--     hFlush stdout
--     input <- getLine
--     let result = parseExpression input
--     case result of
--         Left parseError -> print parseError
--         Right expression -> print (evaluate expression)

main :: IO ()
main = do
    print $ evaluate $ Let "x" (Literal 1) (Variable "x")
    print $ evaluate $ IndexExpression (Record (Map.fromList [("x", Literal 1), ("y", Literal 2)])) 1
    print $ evaluate $ Modify (Record (Map.fromList [("x", Literal 1), ("y", Literal 2)])) 1 (Literal 3)