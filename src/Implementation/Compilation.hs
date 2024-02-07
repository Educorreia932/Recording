module Implementation.Compilation where

import Data.Map qualified as Map
import Explicit.Terms qualified as E
import Explicit.Types
import Implementation.Terms qualified as I

type IndexType = (String, Type)
type IndexAssignment = [IndexType]
type TypeAssignment = [(String, Type)]

indexSet :: Either Kind Type -> IndexAssignment
indexSet (Left Universal) = []
indexSet (Left (RecordKind m)) = Map.toList m
indexSet (Right (ForAll _ k t)) = indexSet (Left k) ++ indexSet (Right t)
indexSet _ = []

compile :: IndexAssignment -> TypeAssignment -> E.Expression -> I.Expression
compile l t = comp
  where
   comp (E.Variable x t) =
      let
       in I.Variable x
   comp (E.Literal i) = I.Literal i
   comp (E.String s) = I.String s
   comp (E.Abstraction x t e) = I.Abstraction x (comp e)
   comp (E.Application e1 e2) = I.Application (comp e1) (comp e2)
   comp (E.ERecord r) = I.Record (fmap comp r)
   comp (E.Dot e _ x) =
      let c = comp e
          index = Right "I"
       in I.IndexExpression c index
   comp (E.Modify e1 i e2) =
      let c1 = comp e1
          c2 = comp e2
       in I.Modify c1 i c2
   comp (E.Poly e t') =
      let
         l' = l ++ indexSet (Right t')
         c1 = compile l' t e
         idxSet = indexSet (Right t')
       in
         foldl (\acc _ -> I.Abstraction "I" acc) c1 idxSet
   comp (E.Let x _ e1 e2) =
      let
         c1 = comp e1
         c2 = comp e2
       in
         I.Let x c1 c2
