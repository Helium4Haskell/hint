module Main
where



import Data.Maybe
import Graphics.UI.WX
import System.IO.Error
import CommandlineOpts
import Interpreter
import Path
import System.Environment
import TextCtrlConsole
import StringOps



-- start of the application.
main :: IO ()
main
  = start hint


-- sets up the main GUI of the application.
hint :: IO ()
hint
  = do f <- frame [text := "Hint", visible := False ]
       c <- textCtrlConsole f
       set f [ layout     := fill $ getLayout c
             , clientSize := size 600 400
             , visible    := True
             ]
       initHintConsole c f
       setFocus c



-- initialise the helium console. Sets the initial text, adds event handlers.
initHintConsole :: Closeable frame => TextCtrlConsole -> frame -> IO ()
initHintConsole console frame
  = do clear console
       addData console SpecialStyle Nothing "Welcome to Hint, the interactive shell to Helium.\n\n"

       installdir <- getInstallationDirectory
       -- let installdir = "C:\\Program Files\\Helium"
       libdir <- pathAdd installdir "lib"
       bindir <- pathAdd installdir "bin"

       -- check writable temp directory
       (ok, tempDir) <- checkTempDirectoryWriteable
       when (not ok) (fail ("The temporary directory is not writeable: " ++ tempDir))

       interpreter <- create (hintOnOutput console) (hintOnFinish console) [libdir] [bindir]

       set console [ rememberFutureInput := True
                   , userInputHandler    := hintOnCommand interpreter frame
                   , displayPrompt       := True
                   , prompt              := "Prelude> "
                   ]

       hintOnFinish console interpreter
  `catch`
    (hintErrorHandler console True)


-- executes the hint command entered by the user.
hintOnCommand :: Closeable frame => Interpreter -> frame -> TextCtrlConsole -> Maybe String -> IO ()
hintOnCommand interpreter frame console Nothing
  = do reset interpreter
hintOnCommand interpreter frame console (Just command)
  = do state <- varGet interpreter
       when (isJust $ running state)
         $ do -- send the input to the running process
              send interpreter (command ++ "\n")
       when (not $ isJust $ running state)
         $ do let cmd = trimSpaces command
              when (not (null cmd))
                $ do when (head cmd == ':')
                       $ do let tokens = words (tail cmd)
                            performCommand tokens
                     when (head cmd /= ':')
                       $ do -- run the interpreter on the expression
                            set console [ rememberFutureInput := False
                                        , displayPrompt       := False
                                        ]
                            evaluate (control console) interpreter False False False command
                            return ()


  `catch`
    (hintErrorHandler console False)
  where
    performCommand :: [String] -> IO ()
    performCommand ("t" : expr@(_:_))
      = do set console [ rememberFutureInput := False
                       , displayPrompt       := False
                       ]
           compileForType (control console) interpreter (unwords expr) -- note: spaces in the expression can occur...
    performCommand ("l" : mod@(_:_))
      = do set console [ rememberFutureInput := False
                       , displayPrompt       := False
                       ]
           loadModule (control console) interpreter (unwords mod) -- note: spaces is filename should not occur.
    performCommand (["l"])
      = do set console [ rememberFutureInput := False
                       , displayPrompt       := False
                       ]
           loadModule (control console) interpreter "Prelude"
    performCommand ("a" : mod@(_:_))
      = alsoLoadModule interpreter (unwords mod)
    performCommand ("u" : mod@(_:_))
      = unloadModule interpreter (unwords mod)
    performCommand (["q"])
      = do reset interpreter
           close frame
    performCommand (["?"])
      = addData console SpecialStyle Nothing
      $  ":?               this text\n"
      ++ ":l [modulename]  (re)loads this module\n"
      ++ ":a <modulename>  additionally loads this module\n"
      ++ ":u <modulename>  unload this additional loaded module\n"
      ++ ":t <expression>  infer type of expression\n"
      ++ ":q               quit hint\n"
    performCommand _
      = addData console ErrorStyle Nothing "Unkown command. Use :? to see a list of console commands.\n"


-- called when the interpreter has some output to publish on the console.
hintOnOutput :: TextCtrlConsole -> Interpreter -> InterpreterOutput -> IO ()
hintOnOutput console interpreter output
  = case output of
      NormalOutput  s   -> addData console NormalStyle  Nothing        s
      WarningOutput s m -> addData console SpecialStyle (linkAction m) s
      ErrorOutput   s m -> addData console ErrorStyle   (linkAction m) s
  `catch`
    (hintErrorHandler console True)
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
      `catch`
        (hintErrorHandler console True)


-- called when the interpreter has finished executing the command.
-- prepares the GUI to accept new commands.
hintOnFinish :: TextCtrlConsole -> Interpreter -> IO ()
hintOnFinish console interpreter
  = do reset interpreter
       state <- varGet interpreter
       set console [ rememberFutureInput := True
                   , prompt              := (currentModule state ++ "> ")
                   , displayPrompt       := True
                   ]
       return ()
  `catch`
    (hintErrorHandler console True)


-- displays the io-error in an error dialog.
hintErrorHandler :: TextCtrlConsole -> Bool -> IOError -> IO ()
hintErrorHandler console rethrow error
  = do errorDialog (control console) "Error" (ioeGetErrorString error)
       when rethrow (ioError error)
       return ()

