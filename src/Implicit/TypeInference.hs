module Implicit.TypeInference where

import Control.Monad.State
import Data.Map qualified as Map

import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implicit.Parser
import Implicit.Terms

type KindAssignment = Map.Map String T.Kind
type TypeAssignment = Map.Map String T.Type

type CompilationState = (KindAssignment, TypeAssignment)

infer' :: Expression -> State CompilationState E.Expression
-- Constants
infer' (Literal a) = return $ E.Literal a
infer' (String a) = return $ E.String a

-- Variable
infer' (Variable x) =
    do
        (_, typeAssignment) <- get
        let t = typeAssignment Map.! x
        return $ E.Variable x []

-- Abstraction
infer' (Abstraction x e) =
    do
        (kindAssign, typeAssign) <- get
        let
            freshType = T.Parameter ("t" ++ show (Map.size typeAssign))
            kindAssign' = Map.insert x T.Universal kindAssign
            typeAssign' = Map.insert x freshType typeAssign
        _ <- put (kindAssign', typeAssign')
        return $ E.Abstraction

infer :: String -> E.Expression
infer s = evalState (infer' expression) startState
  where
    startState = (Map.empty, Map.empty)
    expression = parseExpression s