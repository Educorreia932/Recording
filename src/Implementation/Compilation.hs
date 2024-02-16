module Implementation.Compilation (compile) where

import Data.Map qualified as Map
import Explicit.Parser
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implementation.Terms qualified as I

type IndexType = (String, T.Type)
type IndexAssignment = [IndexType]
type TypeAssignment = Map.Map String T.Type

indexSet :: Either T.Kind T.Type -> IndexAssignment
indexSet (Left T.Universal) = []
indexSet (Left (T.RecordKind m)) = Map.toList m
indexSet (Right (T.ForAll _ k t)) = indexSet (Left k) ++ indexSet (Right t)
indexSet _ = []

idx :: T.Type -> String -> Maybe Int
idx (T.Record r) l = Just $ Map.findIndex l r + 1
idx _ _ = Nothing

compile' :: IndexAssignment -> TypeAssignment -> E.Expression -> I.Expression
compile' indexAssign typeAssign = comp
  where
   -- Variable
   comp (E.Variable x t) = I.Variable x
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
   comp (E.Dot e t x) =
      let c = comp e
          index = case idx t x of
            Just i -> Left i
            Nothing -> Right "I"
       in I.IndexExpression c index
   -- Modify
   comp (E.Modify e1 i e2) =
      let c1 = comp e1
          c2 = comp e2
       in I.Modify c1 i c2
   -- Generic
   comp (E.Poly e t) =
      let
         indexAssign' = indexAssign ++ indexSet (Right t)
         c1 = compile' indexAssign' typeAssign e
         idxSet = indexSet (Right t)
       in
         foldl (\acc _ -> I.IndexAbstraction (Right "I") acc) c1 idxSet
   -- Let expression
   comp (E.Let x _ e1 e2) =
      let
         c1 = comp e1
         c2 = comp e2
       in
         I.Let x c1 c2

compile :: String -> I.Expression
compile = compile' [] Map.empty . parseExpression