module Main
where


import Graphics.UI.WX
import TextCtrlConsole
import Interpreter


-- start of the application.
main :: IO ()
main
  = start hint


-- sets up the main GUI of the application.
hint :: IO ()
hint
  = do f <- frame [text := "Welcome to Hint"]
       c <- textCtrlConsole f
       set f [ layout     := fill $ getLayout c
             , clientSize := size 600 400
             , visible    := True
             ]
       initHintConsole c
       setFocus c


-- initialise the helium console. Sets the initial text, adds event handlers.
initHintConsole :: Console c => c -> IO ()
initHintConsole console
  = do clear console
       addData console SpecialStyle Nothing "Welcome to Hint, the interactive shell to Helium.\n\n"

       interpreter <- create (hintOnOutput console) (hintOnFinish console) [] []

       set console [ rememberFutureInput := True
                   , userInputHandler    := hintOnCommand interpreter
                   , displayPrompt       := True
                   , prompt              := "Prelude> "
                   ]


-- executes the hint command entered by the user.
hintOnCommand :: Console c => Interpreter -> c -> String -> IO ()
hintOnCommand interpreter console command
  = do putStrLn ("got command: " ++ command)
       return ()


-- called when the interpreter has some output to publish on the console.
hintOnOutput :: Console c => c -> Interpreter -> InterpreterOutput -> IO ()
hintOnOutput console interpreter output
  = case output of
      NormalOutput  s -> addData console NormalStyle  Nothing s
      WarningOutput s -> addData console SpecialStyle Nothing s
      ErrorOutput   s -> addData console ErrorStyle   Nothing s



-- called when the interpreter has finished executing the command.
hintOnFinish :: Console c => c -> Interpreter -> IO ()
hintOnFinish console interpreter
  = return ()

