module Interpreter
where



-- Contains the state of the interpreter.
data Interpreter
  = Interpreter


-- | The action the interpreter is currently performing.
data Action
  = None
  | Load FilePath
  | Eval String
  | TypeOf String



-- | Resets the interpreter to the default state.
reset :: Interpreter -> IO Interpreter
reset
  = undefined


-- | Starts with loading a module.
startLoadModule :: FilePath -> Interpreter -> IO Interpreter
startLoadModule
  = undefined


-- | Called when loading of a module is finished.
endLoadModule :: Interpreter -> IO Interpreter
endLoadModule
  = undefined.


-- | Starts the evaluation of an expression.
startEvaluate :: String -> Interpreter -> IO Interpreter
startEvaluate
  = undefined


-- | Called when the evaluation of an expression is finished.
endEvaluate :: Interpreter -> IO Interpreter
endEvaluate
  = undefined


-- | Asks for the type of an expression.
startAskType :: String -> Interpreter -> IO Interpreter
startAskType
  = undefined


-- | Called when asking for a type is finished.
endAskType :: Interpreter -> IO Interpreter
endAskType
  = undefined

