module Implementation.Compilation (compile) where

import Control.Monad.State
import Data.Bifunctor (second)
import Data.List (find)
import Data.Map qualified as Map
import Debug.Trace
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implementation.Terms qualified as I

type IndexType = (String, T.Type)

type IndexAssignment = Map.Map String IndexType

type TypeAssignment = Map.Map String T.Type

class Indexable a where
    indexSet :: a -> [IndexType]

instance Indexable T.KindedType where
    indexSet (_, T.Universal) = []
    indexSet (t, T.RecordKind m1 m2) = map (\(l, _) -> (l, T.Parameter t)) (Map.toList m1 ++ Map.toList m2)

instance Indexable T.Type where
    indexSet (T.ForAll (t, k) s) = indexSet (t, k) ++ indexSet s
    indexSet _ = []

instance Indexable T.Kind where
    indexSet T.Universal = []
    indexSet (T.RecordKind m1 _) = Map.toList m1

-- Calculates the offset of an index assignment for an extension/contraction type
indexOffset :: String -> T.Type -> Int
indexOffset l (T.Extension t1 l' _)
    | l < l' = 0
    | otherwise = 1 + indexOffset l t1
indexOffset l (T.Contraction t1 l' _)
    | l < l' = 0
    | otherwise = -1 + indexOffset l t1
indexOffset _ _ = 0

-- Gets the base type for an extension/contraction type
baseType :: T.Type -> T.Type
baseType (T.Extension t _ _) = baseType t
baseType (T.Contraction t _ _) = baseType t
baseType t = t

insertionIndex :: [String] -> String -> Int
insertionIndex [] _ = 1
insertionIndex (x : xs) l
    | l > x = 1 + insertionIndex xs l
    | otherwise = 1

-- Finds the index of a label in an index assignment
idx :: IndexType -> IndexAssignment -> Maybe I.Index
idx (l, T.Record r) _ = Just $ Left $ insertionIndex (Map.keys r) l
idx (l, T.Contraction t1 l' t2) indexAssign = case idx (l, t1) indexAssign of
    Just (Left n) -> Just $ Left $ n + offset
    Just (Right (i', offset')) -> Just $ Right (i', offset' + offset)
    Nothing -> Nothing
  where
    offset = if l' < l then -1 else 0
idx (l, T.Extension t1 l' t2) indexAssign = case idx (l, t1) indexAssign of
    Just (Left n) -> Just $ Left $ n + offset
    Just (Right (i', offset')) -> Just $ Right (i', offset' + offset)
    Nothing -> Nothing
  where
    offset = if l' < l then 1 else 0
idx (l, t) indexAssign =
    find (\(_, idxType) -> idxType == (l, baseType t)) (Map.toList indexAssign)
        >>= \(i, _) -> Just $ Right (i, offset)
  where
    offset = indexOffset l t

type CompilationState = (IndexAssignment, TypeAssignment)

compile' :: E.Expression -> State CompilationState I.Expression
-- Variable
compile' (E.Variable x typeInstances)
    | null typeInstances = return $ I.Variable x
    | otherwise = do
        (indexAssign, typeAssign) <- get
        let t = typeAssign Map.! x
            substitutionMap = Map.fromList $ zip (map (T.Parameter . fst) (T.typeParameters t)) typeInstances
            indexes = map (second (substitutionMap Map.!)) (indexSet t)
        return $
            foldl
                ( \acc (l, s') -> case idx (l, s') indexAssign of
                    Just i -> I.IndexApplication acc i
                    Nothing -> acc
                )
                (I.Variable x)
                indexes

-- Constants
compile' (E.String s) = return $ I.String s
compile' (E.Literal n) = return $ I.Literal n
-- Abstraction
compile' (E.Abstraction x t e) = do
    (indexAssign, typeAssign) <- get
    let typeAssign' = Map.insert x t typeAssign
    _ <- put (indexAssign, typeAssign')
    c <- compile' e
    return $ I.Abstraction x c

-- Application
compile' (E.Application e1 e2) = do
    c1 <- compile' e1
    c2 <- compile' e2
    return $ I.Application c1 c2

-- Record
compile' (E.Record r) = do
    r' <- Map.elems <$> mapM compile' r -- Map.elems returns sorted by labels
    return $ I.Record r'

-- Dot
compile' (E.Dot e t l) = do
    c <- compile' e
    (indexAssign, _) <- get
    let index = case idx (l, t) indexAssign of
            Just i -> i
            Nothing -> error $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.IndexExpression c index

-- Modify
compile' (E.Modify e1 t l e2) = do
    c1 <- compile' e1
    c2 <- compile' e2
    (indexAssign, _) <- get
    let index = case idx (l, t) indexAssign of
            Just i -> i
            Nothing -> error $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.Modify c1 index c2

-- Generic
compile' (E.Poly e t) = do
    (indexAssign, typeAssign) <- get
    let idxSet = indexSet t
        lastIndex = Map.size indexAssign
        freshIndexes = map (\x -> "I" ++ show x) [lastIndex + 1 .. lastIndex + length idxSet]
        freshIndexesAssign = Map.fromList $ zip freshIndexes idxSet
        indexAssign' = indexAssign `Map.union` freshIndexesAssign
    _ <- put (indexAssign', typeAssign)
    c <- compile' e
    return $ foldl (flip I.IndexAbstraction) c (reverse freshIndexes)

-- Let expression
compile' (E.Let x t e1 e2) = do
    (indexAssign, typeAssign) <- get
    let typeAssign' = Map.insert x t typeAssign
    _ <- put (indexAssign, typeAssign')
    c1 <- compile' e1
    c2 <- compile' e2
    return $ I.Let x c1 c2

-- Contract
compile' (E.Contract e t l) = do
    c <- compile' e
    (indexAssign, _) <- get
    let index = case idx (l, t) indexAssign of
            Just i -> i
            Nothing -> error $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.Contraction c index

-- Extend
compile' (E.Extend e1 t l e2) = do
    c1 <- compile' e1
    c2 <- compile' e2
    (indexAssign, _) <- get
    let index = case idx (l, t) indexAssign of
            Just i -> i
            Nothing -> error $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.Extend c1 index c2

class Compilable a where
    compile :: a -> I.Expression

instance Compilable E.Expression where
    compile e = evalState (compile' e) startState
      where
        startState = (Map.empty, Map.empty)
