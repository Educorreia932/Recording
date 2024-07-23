{-# LANGUAGE LambdaCase #-}

module Implementation.Evaluator where

import Control.Monad.Except
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Errors (RecordingException (EvaluationError))
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

type EvalMonad a = Either RecordingException a

class Substitutes a where
    -- | Replace a variable with an expression/index in an expression
    substitute :: String -> a -> Expression -> EvalMonad a

instance Substitutes Index where
    substitute :: String -> Index -> Expression -> EvalMonad Index
    substitute _ i@(Left _) _ = return i
    substitute var i@(Right (i', offset)) e = do
        evaluatedExpr <- evaluate e
        return $ case evaluatedExpr of
            -- Replace with an index literal
            Literal n
                | var == i' -> Left $ n + offset
                | otherwise -> i
            -- Replace with an index variable
            String s
                | var == i' -> Right (s, offset)
                | otherwise -> i
            _ -> i

instance Substitutes Expression where
    -- Replaces var with e in e'
    substitute var e expr = do
        evaluatedExpr <- evaluate e
        sub evaluatedExpr expr
      where
        sub _ (Literal i) = return $ Literal i
        sub _ (String s) = return $ String s
        sub evaluatedExpr (Variable v)
            | var == v = return evaluatedExpr
            | otherwise = return $ Variable v
        sub evaluatedExpr (IndexAbstraction i body)
            | var == i = return $ IndexAbstraction i body
            -- α-conversion
            | i `elem` freeVariables evaluatedExpr = do
                let i' = disambiguate i
                body' <- substitute i (Variable i') body
                IndexAbstraction i' <$> sub evaluatedExpr body'
            | otherwise = IndexAbstraction i <$> sub evaluatedExpr body
        sub evaluatedExpr (Abstraction v body)
            | var == v = return $ Abstraction v body
            -- α-conversion
            | v `elem` freeVariables evaluatedExpr = do
                let v' = disambiguate v
                body' <- substitute v (Variable v') body
                Abstraction v' <$> sub evaluatedExpr body'
            | otherwise = Abstraction v <$> sub evaluatedExpr body
        sub evaluatedExpr (Application e1 e2) =
            Application <$> sub evaluatedExpr e1 <*> sub evaluatedExpr e2
        sub evaluatedExpr (IndexApplication e1 i) =
            IndexApplication <$> sub evaluatedExpr e1 <*> substitute var i evaluatedExpr
        sub evaluatedExpr (IndexExpression e' i) =
            IndexExpression <$> sub evaluatedExpr e' <*> substitute var i evaluatedExpr
        sub evaluatedExpr (Record m) =
            Record <$> mapM (sub evaluatedExpr) m
        sub evaluatedExpr (Modify r i e') =
            Modify <$> sub evaluatedExpr r <*> substitute var i evaluatedExpr <*> sub evaluatedExpr e'
        sub evaluatedExpr (Let v e1 e2) =
            Let v <$> sub evaluatedExpr e1 <*> sub evaluatedExpr e2
        sub evaluatedExpr (Contraction e' i) =
            Contraction <$> sub evaluatedExpr e' <*> substitute var i evaluatedExpr
        sub evaluatedExpr (Extend e1 i e2) =
            Extend <$> sub evaluatedExpr e1 <*> substitute var i evaluatedExpr <*> sub evaluatedExpr e2

evaluate :: Expression -> EvalMonad Expression
-- Constants
evaluate (Literal n) = return $ Literal n
evaluate (String s) = return $ String s
-- Variable
evaluate (Variable v) = return $ Variable v
-- Abstraction
evaluate (Abstraction v e) = return $ Abstraction v e
-- Record
evaluate (Record m) = Record <$> mapM evaluate m
-- Index Abstraction
evaluate (IndexAbstraction i e) = return $ IndexAbstraction i e
-- Modify
evaluate (Modify (Record r) i e) = do
    case i of
        Left i' -> return $ Record $ toList $ Seq.update (i' - 1) e $ Seq.fromList r
        Right _ -> throwError $ EvaluationError "Not implemented"
evaluate (Modify{}) = throwError $ EvaluationError "Modifying non-record"
-- Let expression
evaluate (Let var e1 e2) = evaluate =<< substitute var e1 e2
-- Index expression
evaluate (IndexExpression e i) = do
    case i of
        Left i' ->
            evaluate e >>= \case
                Record m -> return $ m !! (i' - 1)
                _ -> throwError $ EvaluationError "Indexing non-record"
        _ -> throwError $ EvaluationError "Invalid index"
-- Application
evaluate (Application fun arg) = do
    evaluate fun >>= \case
        Abstraction var body -> evaluate =<< substitute var arg body
        other -> return $ Application other arg
-- Index application
evaluate (IndexApplication fun index) =
    evaluate fun >>= \case
        IndexAbstraction i body -> do
            let s =
                    substitute
                        i
                        ( case index of
                            Left index' -> Literal index'
                            Right (index', _) -> String index'
                        )
                        body
            evaluatedS <- s
            evaluate evaluatedS
        other -> return $ IndexApplication other index
-- Contraction
evaluate (Contraction e i) =
    case i of
        Left i' ->
            evaluate e >>= \case
                Record r -> return $ Record $ remove (i' - 1) r
                _ -> throwError $ EvaluationError "Indexing non-record"
        _ -> throwError $ EvaluationError "Invalid index"
-- Extend
evaluate (Extend e1 i e2) =
    case i of
        Left i' ->
            evaluate e1 >>= \case
                Record r -> do
                    e2' <- evaluate e2
                    return $ Record $ insertAt (i' - 1) r e2'
                _ -> throwError $ EvaluationError "Extend non-record"
        _ -> throwError $ EvaluationError "Invalid index"
