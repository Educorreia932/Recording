module Implementation.Compilation (compile) where

import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
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
   indexSet (t, T.RecordKind m) = Set.fromList $ map (\(l, _) -> (l, T.Parameter t)) (Map.toList m)

instance Indexable T.Type where
   indexSet (T.ForAll (t, k) s) = indexSet (t, k) `Set.union` indexSet s
   indexSet _ = Set.empty

instance Indexable T.Kind where
   indexSet T.Universal = Set.empty
   indexSet (T.RecordKind m) = Set.fromList $ Map.toList m

idx :: T.Type -> String -> IndexAssignment -> Maybe I.Index
idx (T.Record r) l _ = Just $ Left $ Map.findIndex l r + 1
idx t l indexAssign =
   find (\(_, idxType) -> idxType == (l, t)) (Map.toList indexAssign)
      >>= \(i, _) -> Just $ Right i

compile' :: IndexAssignment -> TypeAssignment -> E.Expression -> I.Expression
compile' indexAssign typeAssign = comp
  where
   -- Variable
   comp (E.Variable x typeInstances)
      | null typeInstances =
         I.Variable x
      | otherwise =
         let
            -- Type of the variable
            t :: T.Type
            t = typeAssign Map.! x

            -- Type substitution
            s :: [(String, T.Type)]
            s = zip (T.typeParameters t) typeInstances

            -- Label
            l :: String
            l = fst $ Set.elemAt 0 $ indexSet t
          in
            foldl
               ( \acc (_, s') -> case idx s' l indexAssign of
                  Just i -> I.IndexApplication acc i
                  Nothing -> acc
               )
               (I.Variable x)
               s
   -- Constants
   comp (E.Literal i) = I.Literal i
   comp (E.String s) = I.String s
   -- Abstraction
   comp (E.Abstraction x t e) =
      let typeAssign' = Map.insert x t typeAssign
       in I.Abstraction x (compile' indexAssign typeAssign' e)
   -- Application
   comp (E.Application e1 e2) = I.Application (comp e1) (comp e2)
   -- Record
   comp (E.ERecord r) = I.Record (fmap comp r)
   -- Dot
   comp (E.Dot e t l) =
      let c = comp e
          index = case idx t l indexAssign of
            Just i -> i
            Nothing ->
               error
                  $ "Label \""
                  ++ l
                  ++ "\" not found in "
                  ++ show t
       in I.IndexExpression c index
   -- Modify
   comp (E.Modify e1 t l e2) =
      let c1 = comp e1
          c2 = comp e2
          index = case idx t l indexAssign of
            Just i -> i
            Nothing -> error $ "Label \"" ++ l ++ "\" not found in " ++ show t
       in I.Modify c1 index c2
   -- Generic
   comp (E.Poly e t) =
      let
         idxSet :: Set.Set (String, T.Type)
         idxSet = indexSet t

         freshIndexes :: [String]
         freshIndexes = map (\x -> "I" ++ show x) [(Map.size indexAssign + 1) .. (Set.size idxSet)]

         freshIndexesAssign :: IndexAssignment
         freshIndexesAssign = Map.fromList $ zip freshIndexes (Set.toList idxSet)

         indexAssign' :: IndexAssignment
         indexAssign' = indexAssign `Map.union` freshIndexesAssign

         c1 = compile' indexAssign' typeAssign e
       in
         foldl (flip I.IndexAbstraction) c1 freshIndexes
   -- Let expression
   comp (E.Let x t e1 e2) =
      let
         typeAssign' = Map.insert x t typeAssign
         c1 = comp e1
         c2 = compile' indexAssign typeAssign' e2
       in
         I.Let x c1 c2

compile :: String -> I.Expression
compile = compile' Map.empty Map.empty . parseExpression