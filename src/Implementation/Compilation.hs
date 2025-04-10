module Implementation.Compilation (compile) where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (second)
import Data.List (find)
import Data.Map qualified as Map
import Errors
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
indexOffset l (T.Contraction t1 l' _)
    | l <= l' = 0
    | otherwise = -1 + indexOffset l t1
indexOffset l (T.Extension t1 l' _)
    | l <= l' = 0
    | otherwise = 1 + indexOffset l t1
indexOffset _ _ = 0

-- Gets the base type for an extension/contraction type
baseType :: T.Type -> T.Type
baseType (T.Contraction t _ _) = baseType t
baseType (T.Extension t _ _) = baseType t
baseType t = t

insertionIndex :: [String] -> String -> Int
insertionIndex [] _ = 1
insertionIndex (x : xs) l
    | l > x = 1 + insertionIndex xs l
    | otherwise = 1

-- Finds the index of a label in an index assignment
idx :: IndexType -> IndexAssignment -> Maybe I.Index
-- If it is a record, calculates the index of the label
idx (l, T.Record r) _ = Just $ Left $ insertionIndex (Map.keys r) l
-- If it is a parameter, finds the index in the index assignment
idx (l, t@(T.Parameter _)) indexAssign = case find (\(_, (l', t')) -> (l, t) == (l', t')) (Map.toList indexAssign) of
    Just (i, _) -> Just $ Right (i, 0)
    Nothing -> Nothing
-- Otherwise, it is an extension/contraction, calculates the index/offset of the label
idx (l, t) indexAssign = case idx (l, baseType t) indexAssign of
    Just (Left i) -> Just $ Left $ i + offset
    Just (Right (i, _)) -> Just $ Right (i, offset)
    Nothing -> Nothing
  where
    offset = indexOffset l t

type CompilationState = (IndexAssignment, TypeAssignment)

compile' :: E.Expression -> ExceptT RecordingException (State CompilationState) I.Expression
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
-- List
compile' (E.List l) = I.List <$> mapM compile' l
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
    index <- case idx (l, t) indexAssign of
        Just i -> return i
        Nothing -> throwError $ CompilationError $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.IndexExpression c index

-- Modify
compile' (E.Modify e1 t l e2) = do
    c1 <- compile' e1
    c2 <- compile' e2
    (indexAssign, _) <- get
    index <- case idx (l, t) indexAssign of
        Just i -> return i
        Nothing -> throwError $ CompilationError $ "Label \"" ++ l ++ "\" not found in " ++ show t
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
    index <- case idx (l, t) indexAssign of
        Just i -> return i
        Nothing -> throwError $ CompilationError $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.Contraction c index

-- Extend
compile' (E.Extend e1 t l e2) = do
    c1 <- compile' e1
    c2 <- compile' e2
    (indexAssign, _) <- get
    index <- case idx (l, t) indexAssign of
        Just i -> return i
        Nothing -> throwError $ CompilationError $ "Label \"" ++ l ++ "\" not found in " ++ show t
    return $ I.Extend c1 index c2

compile :: E.Expression -> Either RecordingException I.Expression
compile e = do
    let initialState = (Map.empty, Map.empty)
    evalState (runExceptT $ compile' e) initialState
