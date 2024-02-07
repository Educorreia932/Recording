module Main where

import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Explicit.Parser (parseExpression)
import Explicit.Terms
import Explicit.Types qualified as T
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
            T.ForAll
                "t1"
                T.Universal
                ( T.ForAll
                    "t2"
                    (T.RecordKind (Map.singleton "Name" (T.Parameter "t1")))
                    (T.Parameter "t1" `T.Arrow` T.Parameter "t2")
                )
    let p =
            Let
                "name"
                t
                ( Poly
                    (Abstraction "x" (T.Parameter "t2") (Dot (Variable "x" []) t "Name"))
                    t
                )
                ( Variable "name" [T.String, T.Record (Map.fromList [("Name", T.String), ("Office", T.Int)])]
                    `Application` ERecord
                        ( OMap.fromList
                            [ ("Name", String "Joe")
                            , ("Office", Literal 403)
                            ]
                        )
                )

    putStrLn $ "[Explicit]        " ++ show p
    putStrLn $ "[Implementation]  " ++ show (compile [] [] p)