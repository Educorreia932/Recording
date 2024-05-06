module Repl where

import Control.Monad.IO.Class
import Implementation.Compilation
import Implementation.Evaluator
import Implicit.Parser (parseExpression)
import Implicit.TypeInference (typeInference)
import System.Console.Repline

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd = liftIO . print . parseExpression

completer :: (Monad m) => WordCompleter m
completer _ = return []

-- Commands
help :: (MonadIO m, Show a) => a -> m ()
help _ = liftIO $ do
    putStrLn "Commands available from the prompt:"
    putStrLn "    <expression>   Show an expression"
    putStrLn "    :quit,  :q     Exit the REPL"
    putStrLn "    :eval,  :e     Evaluate an expression"
    putStrLn "    :type,  :t     Infer the type of an expression"
    putStrLn "    :infer, :i     Explicity types an expression"
    putStrLn "    :help,  :h     Display this help message"

ini :: Repl ()
ini = liftIO $ putStrLn "λχ interpreter. Type :h for help"

opts :: Options (HaskelineT IO)
opts =
    [ -- Help
      ("h", help)
    , ("help", help)
    , -- Quit
      ("q", const abort)
    , ("quit", const abort)
    , -- Type
      ("t", type')
    , ("type", type')
    , -- Infer
      ("i", infer')
    , ("infer", infer')
    , -- Compile
      ("c", compile')
    , ("compile", compile')
    , -- Evaluate
      ("e", eval')
    , ("eval", eval')
    ]
  where
    type' :: String -> Repl ()
    type' = liftIO . print . snd . typeInference

    infer' :: String -> Repl ()
    infer' = liftIO . print . fst . typeInference

    compile' :: String -> Repl ()
    compile' = liftIO . print . compile . fst . typeInference

    eval' :: String -> Repl ()
    eval' = liftIO . print . evaluate . compile . fst . typeInference

final :: Repl ExitDecision
final = return Exit

repl :: IO ()
repl =
    evalReplOpts $
        ReplOpts
            { banner = const (pure "λ> ")
            , command = cmd
            , options = opts
            , prefix = Just ':'
            , multilineCommand = Nothing
            , tabComplete = Word0 completer
            , initialiser = ini
            , finaliser = final
            }