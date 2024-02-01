module Main where

import Control.Monad (forever)
import Data.Map qualified as Map
import Explicit.Terms
import Explicit.Types
import Implementation.Compilation
import Implementation.Evaluator (evaluate)
import System.IO (hFlush, stdout)

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
    -- λx.x.Name
    let t =
            ForAll
                "t1"
                Universal
                ( ForAll
                    "t2"
                    (RecordKind (Map.singleton "Name" (Parameter "t1")))
                    (Parameter "t2" `Arrow` Parameter "t1")
                )
    let program =
            Poly
                (Abstraction "x" (Parameter "t2") (Dot (Variable "x") t "Name"))
                t

    putStrLn $ "[Explicit]        " ++ show program
    putStrLn $ "[Implementation]  " ++ show (compile [] [] program)