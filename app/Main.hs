module Main where

data E
  = Application E E
  | Abstraction E
  | Variable Int  -- n refers to the variable bound by the nth-innermost abstraction
  | Literal Int   -- literals
  deriving (Show)

eval :: E -> E
eval x = x

main :: IO ()
main =
    print $ eval i
        where i = Abstraction (Abstraction (Variable 1))
