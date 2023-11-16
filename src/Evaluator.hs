module Evaluator where

import Common (Expression (..), VariableIdentifier)

substitute :: VariableIdentifier -> Expression -> Expression -> Expression
substitute _ (Literal a) _ = Literal a
substitute x (Variable y) e
    | x == y = e
    | otherwise = Variable y
substitute x (Abstraction y e1) e2
    | x == y = Abstraction y e1
    | otherwise = Abstraction y (substitute x e1 e2)
substitute x (Application e1 e2) e3 = Application (substitute x e1 e3) (substitute x e2 e3)

evaluate :: Expression -> Expression
evaluate (Application (Abstraction x e) v2) = substitute x e v2
evaluate (Application e1 e2) = Application (evaluate e1) e2
evaluate e = e