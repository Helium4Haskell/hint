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

       --
       -- menubar
       --
       mFile <- menuPane [text:= "&File"]
       mOpen <- menuItem mFile [text := "&Open module...", help := "Open a helium module."]
       mReload <- menuItem mFile [text := "&Reload module", help := "Reload the current helium module."]
       menuLine mFile
       mExit <- menuItem mFile [text := "&Exit", help := "Exit Hint - Have a Nice day"]
       
       mInterpreter <- menuPane [text := "&Interpreter"]
       mClearScreen <- menuItem mInterpreter [text := "&Clear console", help := "Clears the console."]
       mTerminate   <- menuItem mInterpreter [text := "&Terminate program", help := "Terminate program."]

       mHelp <- menuHelp []
       mHintCommands <- menuItem mHelp [text := "&Interpreter commands"]
       about <- menuAbout mHelp [help := "About Hint"]

       --
       -- initial text on console
       --
       setInitialConsoleContent c
       set c [ rememberFutureInput := True
             , displayPrompt       := True
             , prompt              := "Prelude> "
             ]

       --
       -- Initialize the interpreter
       --
       interpreter <- initializeInterpreter c
       set c [ userInputHandler := hintOnCommand interpreter f ]
       
       setCommandlinePaths c interpreter

       --
       -- update frame and set event handlers.
       --
       set f [ layout     := fill $ getLayout c
             , clientSize := sz 600 400
             , menubar    := [mFile, mInterpreter, mHelp]
             , on (menu mOpen)   := onOpen interpreter f c
             , on (menu about)   := infoDialog f "About Hint" "Hint - Helium Interpreter\nArie Middelkoop, 2004\nhttp://www.cs.uu.nl/helium\n\nReport bugs and suggestions to:\nhelium@cs.uu.nl"
             , on (menu mExit)   := do reset interpreter
                                       close f
             , on (menu mReload)       := echo c ":r" $ hintOnCommand interpreter f c (Just ":r")
             , on (menu mHintCommands) := echo c ":?" $ hintOnCommand interpreter f c (Just ":?")
             , on (menu mClearScreen)  := clear c
             , on (menu mTerminate)    := reset interpreter
             , visible    := True
             ]

       hintOnFinish c interpreter
       setFocus c
  where
    echo :: TextCtrlConsole -> String -> IO () -> IO ()
    echo console text action
      = do appendData console InputStyle (text ++ "\r\n") (Just False)
           action

    onOpen interpreter f c
      = do -- changes working directory
           mbfname <- fileOpenDialog f True True "Open module" [("Helium module", ["*.hs"])] "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> do name <- getNameOnly fname
                              let cmd = ":l " ++ name
                              echo c cmd $ hintOnCommand interpreter f c (Just cmd)

    initializeInterpreter :: TextCtrlConsole -> IO Interpreter
    initializeInterpreter c
      = do -- check writable temp directory
           (ok, tempDir) <- checkTempDirectoryWriteable
           when (not ok) (fail ("The temporary directory is not writeable: " ++ tempDir))

           interpreter <- create (hintOnOutput c) (hintOnFinish c)
           return interpreter

    setCommandlinePaths :: TextCtrlConsole -> Interpreter -> IO ()
    setCommandlinePaths console interpreter
      = do (heliumPath, lvmPath, editorPath) <- getCommandlineArgs
           varUpdate interpreter (\i -> i { heliumPath = heliumPath
                                          , lvmrunPath = lvmPath
                                          , editorPath = editorPath
                                          })
           return ()
      `catch`
        (hintErrorHandler console False)

    setInitialConsoleContent :: TextCtrlConsole -> IO ()
    setInitialConsoleContent c
      = do clear c
           addData c SpecialStyle Nothing "Welcome to Hint, the interactive shell to Helium.\n\n"


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
                            evaluate (control console) interpreter False False False False command
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
    performCommand (["r"])
      = do set console [ rememberFutureInput := False
                       , displayPrompt       := False
                       ]
           state <- varGet interpreter
           loadModule (control console) interpreter (targetModule state)
    performCommand ("a" : mod@(_:_))
      = alsoLoadModule interpreter (unwords mod)
    performCommand ("u" : mod@(_:_))
      = unloadModule interpreter (unwords mod)
    performCommand (["q"])
      = do reset interpreter
           close frame
    performCommand (["?"])
      = addData console SpecialStyle Nothing
      $  "\n"
      ++ ":?               this text\n"
      ++ ":l [modulename]  (re)loads this module\n"
      ++ ":r               reloads the module\n"
      ++ ":a <modulename>  additionally loads this module\n"
      ++ ":u <modulename>  unload this additional loaded module\n"
      ++ ":t <expression>  infer type of expression\n"
      ++ ":q               quit hint\n"
      ++ "\n"
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
                   , prompt              := (currentModuleName state ++ "> ")
                   , displayPrompt       := True
                   ]
       return ()
  `catch`
    (hintErrorHandler console True)


-- displays the io-error in an error dialog.
hintErrorHandler :: TextCtrlConsole -> Bool -> IOError -> IO ()
hintErrorHandler console True error
  = do errorDialog (control console) "Error" (ioeGetErrorString error)
       ioError error
       return ()
hintErrorHandler console False error
  = do addData console ErrorStyle Nothing ("Hint-error: " ++ (ioeGetErrorString error) ++ "\n")
       return ()

