module Implementation.Evaluator where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Debug.Trace
import Implementation.Compilation
import Implementation.Terms

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_ : xs) = xs
remove n (x : xs) = x : remove (n - 1) xs

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

insertAt :: Int -> [a] -> a -> [a]
insertAt _ [] newElem = [newElem]
insertAt pos (x : xs) newElem
    | pos == 0 = newElem : x : xs
    | pos > 0 = x : insertAt (pos - 1) xs newElem
    | otherwise = x : insertAt (pos + length (x : xs)) xs newElem

disambiguate :: String -> String
disambiguate x = x ++ "'"

class Substitutes a where
    -- | Replace a variable with an expression/index in an expression
    substitute :: String -> a -> Expression -> a

instance Substitutes Index where
    substitute _ i@(Left _) _ = i
    substitute var i@(Right (i', offset)) e = case evaluate e of
        -- Replace with a index literal
        Literal n
            | var == i' -> Left $ n + offset
            | otherwise -> i
        -- Replace with a index variable
        String s
            | var == i' -> Right (s, offset)
            | otherwise -> i
        _ -> i

instance Substitutes Expression where
    -- Replaces var with e in e'
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
        sub (IndexApplication e1 i) = IndexApplication (sub e1) (substitute var i e)
        sub (IndexExpression e' i) = IndexExpression (sub e') (substitute var i e)
        sub (Record m) = Record $ fmap sub m
        sub (Modify r i e') =
            Modify
                (sub r)
                (substitute var i e)
                (sub e')
        sub (Let v e1 e2) = Let v (sub e1) (sub e2)
        sub (Contraction e' i) = Contraction (sub e') (substitute var i e)
        sub (Extend e1 i e2) =
            Extend
                (sub e1)
                (substitute var i e)
                (sub e2)

class Evaluable a where
    evaluate :: a -> Expression

instance Evaluable Expression where
    evaluate :: Expression -> Expression
    -- Constants
    evaluate (Literal n) = Literal n
    evaluate (String s) = String s
    -- Variable
    evaluate (Variable v) = Variable v
    -- Abstraction
    evaluate (Abstraction v e) = Abstraction v e
    -- Record
    evaluate (Record m) = Record $ fmap evaluate m
    -- Index Abstraction
    evaluate (IndexAbstraction i e) = IndexAbstraction i e
    -- Modify
    evaluate (Modify (Record r) i e) =
        case i of
            Left i' -> Record $ toList $ Seq.update (i' - 1) e $ Seq.fromList r
            Right _ -> error "Not implemented"
    evaluate (Modify{}) = error "Modifying non-record"
    -- Let expression
    evaluate (Let var e1 e2) =
        evaluate $ substitute var e1 e2
    -- Index expression
    evaluate (IndexExpression e i) =
        case i of
            Left i' -> case evaluate e of
                Record m -> m !! (i' - 1)
                _ -> error "Indexing non-record"
            _ -> error "Invalid index"
    -- Application
    evaluate (Application fun arg) =
        case evaluate fun of
            Abstraction var body -> evaluate $ substitute var arg body
            other -> Application other arg
    -- Index application
    evaluate (IndexApplication fun index) = case evaluate fun of
        IndexAbstraction i body ->
            let s =
                    substitute
                        i
                        ( case index of
                            Left index' -> Literal index'
                            Right (index', _) -> String index'
                        )
                        body
             in evaluate s
        other -> IndexApplication other index
    -- Contraction
    evaluate (Contraction e i) =
        case i of
            Left i' -> case evaluate e of
                Record r -> Record $ remove (i' - 1) r
                _ -> error "Indexing non-record"
            _ -> error "Invalid index"
    -- Extend
    evaluate (Extend e1 i e2) =
        case i of
            Left i' -> case evaluate e1 of
                Record r -> Record $ insertAt (i' - 1) r (evaluate e2)
                _ -> error "Extend non-record"
            _ -> error "Invalid index"
