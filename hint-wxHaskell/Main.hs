module Main
where



import Data.Maybe
import Graphics.UI.WX
import TextCtrlConsole
import Interpreter
import Path
import System.Environment



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
initHintConsole :: TextCtrlConsole -> IO ()
initHintConsole console
  = do clear console
       addData console SpecialStyle Nothing "Welcome to Hint, the interactive shell to Helium.\n\n"
       
       args <- getArgs
       let installdir = if length args == 1 then head args else "C:\\Program Files\\Helium"
       libdir <- pathAdd installdir "lib"
       bindir <- pathAdd installdir "bin"

       interpreter <- create (hintOnOutput console) (hintOnFinish console) [libdir] [bindir]

       set console [ rememberFutureInput := True
                   , userInputHandler    := hintOnCommand interpreter
                   , displayPrompt       := True
                   , prompt              := "Prelude> "
                   ]


-- executes the hint command entered by the user.
hintOnCommand :: Interpreter -> TextCtrlConsole -> String -> IO ()
hintOnCommand interpreter console command
  = do state <- varGet interpreter
       when (isJust $ running state)
         $ do -- send the input to the running process
              send interpreter (command ++ "\n")
       when (not $ isJust $ running state)
         $ do -- run the interpreter on the expression
              evaluate (control console) interpreter command
              set console [ rememberFutureInput := False
                          , displayPrompt       := False
                          ]
              return ()


-- called when the interpreter has some output to publish on the console.
hintOnOutput :: TextCtrlConsole -> Interpreter -> InterpreterOutput -> IO ()
hintOnOutput console interpreter output
  = case output of
      NormalOutput  s   -> addData console NormalStyle  Nothing        s
      WarningOutput s m -> addData console SpecialStyle (linkAction m) s
      ErrorOutput   s m -> addData console ErrorStyle   (linkAction m) s
  where
    linkAction :: Maybe InFileLink -> Maybe (IO ())
    linkAction m
      = do (modulename, row, column) <- m
           return (onClick modulename row column)

    -- action to perform once the user clicks on a link
    onClick :: String -> Int -> Int -> IO ()
    onClick modulename row column
      = do putStrLn "Performing click command - NOT YET"
           return ()


-- called when the interpreter has finished executing the command.
-- prepares the GUI to accept new commands.
hintOnFinish :: TextCtrlConsole -> Interpreter -> IO ()
hintOnFinish console interpreter
  = do reset interpreter
       set console [ rememberFutureInput := True
                   , displayPrompt       := True
                   ]
       return ()

