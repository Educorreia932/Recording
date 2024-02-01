module Implementation.Evaluator (evaluate) where

import Data.Map.Ordered qualified as Map
import Implementation.Terms (Expression (..), VariableIdentifier)

freeVariables :: Expression -> [VariableIdentifier]
freeVariables (Literal _) = []
freeVariables (Variable v) = [v]
freeVariables (Abstraction v e) = filter (/= v) $ freeVariables e
freeVariables (Application e1 e2) = freeVariables e1 ++ freeVariables e2
freeVariables (Record m) = concatMap freeVariables m

disambiguate :: VariableIdentifier -> VariableIdentifier
disambiguate x = x ++ "'"

substitute :: VariableIdentifier -> Expression -> Expression -> Expression
substitute var e = sub
  where
    sub (Literal l) = Literal l
    sub (Variable v)
        | var == v = e
        | otherwise = Variable v
    sub (Abstraction v body)
        | var == v = Abstraction v body
        -- α-conversion
        | v `elem` freeVariables e =
            let v' = disambiguate v
                body' = substitute v (Variable v') body
             in Abstraction v' $ sub body'
        | otherwise = Abstraction v $ sub body
    sub (Application e1 e2) = Application (sub e1) (sub e2)

evaluate :: Expression -> Expression
evaluate (Let var e1 e2) = evaluate $ substitute var e1 e2
evaluate (Modify (Record r) i e) =
    let (k, _) = case Map.elemAt r i of
            Just x -> x
            Nothing -> error "Index out of bounds"
     in Record $ Map.alter (\_ -> Just e) k r
-- evaluate (IndexExpression e i) = case e of
--     Record m -> case Map.elemAt m i of
--         Just (_, e') -> e'
--         Nothing -> error "Index out of bounds"
--     _ -> error "Indexing non-record"
evaluate (Application fun arg) = case evaluate fun of
    Abstraction var body -> evaluate $ substitute var arg body
    other -> Application other arg
evaluate e = e
