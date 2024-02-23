module Implementation.Evaluator where

import Data.Map.Ordered qualified as OMap
import Implementation.Compilation
import Implementation.Terms

freeVariables :: Expression -> [String]
freeVariables (Literal _) = []
freeVariables (String _) = []
freeVariables (Variable v) = [v]
freeVariables (Abstraction v e) = filter (/= v) $ freeVariables e
freeVariables (Application e1 e2) = freeVariables e1 ++ freeVariables e2
freeVariables (Record m) = concatMap freeVariables m
freeVariables (Modify r _ e) = freeVariables r ++ freeVariables e
freeVariables (Let v e1 e2) = freeVariables e1 ++ filter (/= v) (freeVariables e2)
freeVariables (IndexExpression e _) = freeVariables e
freeVariables (IndexApplication e1 _) = freeVariables e1
freeVariables _ = []

disambiguate :: String -> String
disambiguate x = x ++ "'"

substitute :: String -> Expression -> Expression -> Expression
substitute var e = sub
  where
    sub (Literal i) = Literal i
    sub (String s) = String s
    sub (Variable v)
        | var == v = e
        | otherwise = Variable v
    sub (IndexAbstraction i body)
        | var == i = IndexAbstraction i body
        -- α-conversion
        | i `elem` freeVariables e =
            let i' = disambiguate i
                body' = substitute i (Variable i') body
             in IndexAbstraction i' $ sub body'
        | otherwise = IndexAbstraction i $ sub body
    sub (Abstraction v body)
        | var == v = Abstraction v body
        -- α-conversion
        | v `elem` freeVariables e =
            let v' = disambiguate v
                body' = substitute v (Variable v') body
             in Abstraction v' $ sub body'
        | otherwise = Abstraction v $ sub body
    sub (Application e1 e2) = Application (sub e1) (sub e2)
    sub (IndexApplication e1 e2) = IndexApplication (sub e1) e2
    sub (IndexExpression e' i) = case i of
        Left _ -> IndexExpression (sub e') i
        Right i' -> case e of
            Literal n
                | var == i' -> IndexExpression (sub e') (Left n)
                | otherwise -> IndexExpression (sub e') i
            String s
                | var == i' -> IndexExpression (sub e') (Right s)
                | otherwise -> IndexExpression (sub e') i
            _ -> IndexExpression (sub e') i
    sub (Record m) = Record $ fmap sub m
    sub (Modify r i e') = Modify (sub r) i (sub e')
    sub (Let v e1 e2) = Let v (sub e1) (sub e2)

evaluate' :: Expression -> Expression
-- Modify
evaluate' (Modify (Record r) i e) =
    case i of
        Left i' ->
            let (k, _) = case OMap.elemAt r (i' - 1) of
                    Just x -> x
                    Nothing -> error "Index out of bounds"
             in Record $ OMap.alter (\_ -> Just e) k r
        Right _ -> error "Not implemented"
-- Let expression
evaluate' (Let var e1 e2) = evaluate' $ substitute var e1 e2
-- Index expression
evaluate' (IndexExpression e i) =
    case i of
        Left i' -> case e of
            Record m -> case OMap.elemAt m (i' - 1) of
                Just (_, e') -> e'
                Nothing -> error "Index out of bounds"
            _ -> error "Indexing non-record"
        _ -> error "Invalid index"
-- Application
evaluate' (Application fun arg) = case evaluate' fun of
    Abstraction var body -> evaluate' $ substitute var arg body
    other -> Application other arg
-- Index application
evaluate' (IndexApplication fun index) = case evaluate' fun of
    IndexAbstraction i body ->
        evaluate'
            $ substitute
                i
                ( case index of
                    Left index' -> Literal index'
                    Right index' -> String index'
                )
                body
    other -> IndexApplication other index
evaluate' e = e

evaluate :: String -> Expression
evaluate = evaluate' . compile