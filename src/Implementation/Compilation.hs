module Implementation.Compilation (compile) where

import Control.Monad.State
import Data.List (find)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace
import Explicit.Parser
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implementation.Terms qualified as I

type IndexType = (String, T.Type)
type IndexAssignment = Map.Map String IndexType
type TypeAssignment = Map.Map String T.Type

class Indexable a where
   indexSet :: a -> Set.Set IndexType

instance Indexable T.KindedType where
   indexSet (_, T.Universal) = Set.empty
   indexSet (t, T.RecordKind m1 m2) = Set.fromList $ map (\(l, _) -> (l, T.Parameter t)) (Map.toList m1)

instance Indexable T.Type where
   indexSet (T.ForAll (t, k) s) = indexSet (t, k) `Set.union` indexSet s
   indexSet _ = Set.empty

instance Indexable T.Kind where
   indexSet T.Universal = Set.empty
   indexSet (T.RecordKind m1 _) = Set.fromList $ Map.toList m1

type CompilationState = (IndexAssignment, TypeAssignment)

idx :: IndexType -> IndexAssignment -> Maybe I.Index
idx (l, T.Record r) _ = Just $ Left $ Map.findIndex l r + 1
idx (l, t) indexAssign =
   find (\(_, idxType) -> idxType == (l, t)) (Map.toList indexAssign)
      >>= \(i, _) -> Just $ Right i

compile' :: E.Expression -> State CompilationState I.Expression
-- Variable
compile' (E.Variable x t)
   | null t = return $ I.Variable x
   | otherwise = do
      (indexAssign, typeAssign) <- get
      let
         t' :: T.Type
         t' = typeAssign Map.! x

         s :: [(String, T.Type)]
         s = zip (T.typeParameters t') t

         l :: String
         l = fst $ Set.elemAt 0 $ indexSet t'
      return
         $ foldl
            ( \acc (_, s') -> case idx (l, s') indexAssign of
               Just i -> I.IndexApplication acc i
               Nothing -> acc
            )
            (I.Variable x)
            s

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
       freshIndexes = map (\x -> "I" ++ show x) [lastIndex + 1 .. lastIndex + Set.size idxSet]
       freshIndexesAssign = Map.fromList $ zip freshIndexes (Set.toList idxSet)
       indexAssign' = indexAssign `Map.union` freshIndexesAssign
   _ <- put (indexAssign', typeAssign)
   c <- compile' e
   return $ foldl (flip I.IndexAbstraction) c freshIndexes

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
compile' (E.Extend e1 _ _ e2) = do
   c1 <- compile' e1
   c2 <- compile' e2
   return $ I.Extend c1 c2

compile :: String -> I.Expression
compile s = evalState (compile' expression) startState
  where
   startState = (Map.empty, Map.empty)
   expression = parseExpression s