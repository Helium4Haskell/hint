module Interpreter
where



import Data.Maybe
import IO



-- Contains the state of the interpreter.
data Interpreter
  = Interpreter


-- | The action the interpreter is currently performing.
data Action
  = None
  | Load   FilePath (Bool -> Interpreter -> IO ())
  | Eval   String   (Bool -> Interpreter -> IO ())
  | TypeOf String   (Maybe String -> Interpreter -> IO ())



-- | Resets the interpreter to the default state.
reset :: Interpreter -> IO Interpreter
reset
  = undefined


-- | Starts with loading a module. At the end of the loading the module, it'll
--   execute the given IO function (callback). This function is executed on
--   the same thread that calls endLoadModule.
startLoadModule :: FilePath -> (Bool -> Interpreter -> IO ()) -> Interpreter -> IO Interpreter
startLoadModule
  = undefined


-- | Called when loading of a module is finished.
endLoadModule :: Interpreter -> IO Interpreter
endLoadModule
  = undefined


-- | Starts the evaluation of an expression. At the end of the evalution, it'll
--   execute the given IO function (callback). This function is executed on
--   the same thread that calls endEvaluate.
startEvaluate :: String -> (Bool -> Interpreter -> IO ()) -> Interpreter -> IO Interpreter
startEvaluate
  = undefined


-- | Called when the evaluation of an expression is finished.
endEvaluate :: Interpreter -> IO Interpreter
endEvaluate
  = undefined


-- | Asks for the type of an expression. At the end of the evalution, it'll
--   execute the given IO function (callback). This function is executed on
--   the same thread that calls endEvaluate.
startAskType :: String -> (Maybe String -> Interpreter -> IO ()) -> Interpreter -> IO Interpreter
startAskType
  = undefined


-- | Called when asking for a type is finished.
endAskType :: Interpreter -> IO Interpreter
endAskType
  = undefined

