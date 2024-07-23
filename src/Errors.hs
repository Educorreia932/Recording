module Errors where

data RecordingException
    = ParseError String
    | TypeInferenceError String
    | UnificationError String
    | CompilationError String
    | EvaluationError String
    | OtherError String
    deriving (Eq)

instance Show RecordingException where
    show (ParseError s) = "Parse error: " ++ s
    show (TypeInferenceError s) = "Type inference error: " ++ s
    show (UnificationError s) = "Unification error: " ++ s
    show (CompilationError s) = "Compilation error: " ++ s
    show (EvaluationError s) = "Evaluation error: " ++ s
    show (OtherError s) = "Error: " ++ s