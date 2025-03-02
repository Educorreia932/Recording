module Repl where

import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Errors (RecordingException)
import Explicit.Terms qualified as E
import Explicit.TypeInference (typeInference)
import Explicit.Types (Type)
import Implementation.Compilation (compile)
import Implementation.Evaluation (evaluate)
import Implicit.Parser (parseExpression)
import Implicit.Terms qualified as I
import Prettyprinter
import Prettyprinter.Render.String
import System.Console.Repline
import Pretty

type Repl a = HaskelineT IO a

-- Tab completion
completer :: (Monad m) => WordCompleter m
completer _ = return []

-- Help command
help :: (MonadIO m, Show a) => a -> m ()
help _ = liftIO $ do
  putStrLn "Commands available from the prompt:"
  putStrLn "    <expr>               Show an expression"
  putStrLn "    :q[uit]              Exit the REPL"
  putStrLn "    :t[ype]    <expr>    Infer the type of an expression"
  putStrLn "    :i[nfer]   <expr>    Explicity type an expression"
  putStrLn "    :c[ompile] <expr>    Compile an expression"
  putStrLn "    :e[val]    <expr>    Evaluate an expression"
  putStrLn "    :h[elp]              Display this help message"

-- Starting message
initializer :: Repl ()
initializer = liftIO $ putStrLn "Recording (1.2.0). Type :h to see a list of available commands"

handleParsed :: (Pretty a) => (I.Expression -> Either RecordingException a) -> String -> Repl ()
handleParsed process input = do
  let result = parseExpression input >>= process
  case result of
    Left err -> liftIO $ print err
    Right expr -> liftIO $ putStrLn $ render expr

-- Default command
cmd :: String -> Repl ()
cmd = handleParsed Right

handleAction :: (Pretty a) => ((E.Expression, Type) -> Either RecordingException a) -> String -> Repl ()
handleAction action = handleParsed $ typeInference >=> action

-- List of available commands
commands :: Options (HaskelineT IO)
commands =
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
  type' = handleAction $ \(_, t) -> return t
  infer' = handleAction $ \(expr, _) -> return expr
  compile' = handleAction $ \(expr, _) -> compile expr
  eval' = handleAction $ \(expr, _) -> compile expr >>= evaluate

repl :: IO ()
repl = evalReplOpts replOpts
 where
  replOpts =
    ReplOpts
      { banner = const (pure "λ> ")
      , command = cmd
      , options = commands
      , prefix = Just ':'
      , multilineCommand = Nothing
      , tabComplete = Word0 completer
      , initialiser = initializer
      , finaliser = return Exit
      }
