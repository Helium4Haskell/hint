module Interpreter
where



import Data.Maybe
import IO
import Graphics.UI.WXCore
import Graphics.UI.WX
import System.Directory
import Path
import qualified HeliumOutputParser as O
import StringOps



type Interpreter = Var InterpreterState


data InterpreterState
  = IS { reversedPendingOutput :: [String]
       , compilationCompleted  :: Bool
       , evaluationAborted     :: Bool
       , running               :: Maybe (String -> IO(), Process (), Int)
       , alreadyDead           :: Bool
       , tempFiles             :: [String]
       , sequenceNr            :: Int
       , dataAvailableCallback :: Interpreter -> InterpreterOutput -> IO ()
       , finishCallback        :: Interpreter -> IO ()
       , libraryDirs           :: [FilePath]
       , binaryDirs            :: [FilePath]
       , currentModule         :: String
       , targetModule          :: String
       , additionalModules     :: [String]
       , compileOnly           :: Bool
       , ignoreWarnings        :: Bool
       , showExprType          :: Bool
       }


data InterpreterOutput
  = NormalOutput String
  | WarningOutput String (Maybe InFileLink)
  | ErrorOutput String (Maybe InFileLink)


type InFileLink
  = (String, Int, Int)


interpreterMainModule :: String
interpreterMainModule
  = "HintHeliumInterface"


-- Creates an initial interpreter.
create :: (Interpreter -> InterpreterOutput -> IO ()) -> (Interpreter -> IO ()) -> [FilePath] -> [FilePath] -> IO Interpreter
create onOutput onFinish libDirs binDirs
  = varCreate IS { reversedPendingOutput = []
                 , compilationCompleted  = False
                 , evaluationAborted     = True
                 , running               = Nothing
                 , alreadyDead           = True
                 , tempFiles             = []
                 , sequenceNr            = 0
                 , dataAvailableCallback = onOutput
                 , finishCallback        = onFinish
                 , libraryDirs           = libDirs
                 , binaryDirs            = binDirs
                 , currentModule         = "Prelude"
                 , targetModule          = "Prelude"
                 , additionalModules     = []
                 , compileOnly           = False
                 , ignoreWarnings        = False
                 , showExprType          = False
                 }


-- Runs the expression
evaluate :: Window a -> Interpreter -> Bool -> Bool -> Bool -> String -> IO ()
evaluate window interpreter isCompileOnly isIgnoreWarnings isShowExprType expression
  = let tempfileName = interpreterMainModule ++ ".hs"
     in do reset interpreter  -- abort any (possibly) running operations
           state <- varGet interpreter
           let state' = state { sequenceNr        = sequenceNr state + 1
                              , evaluationAborted = False
                              , compileOnly       = isCompileOnly
                              , ignoreWarnings    = isIgnoreWarnings
                              , showExprType      = isShowExprType
                              }

           -- build tempfile
           tempfile <- getTempFilename tempfileName
           writeMainModule tempfile expression (additionalModules state' ++ [targetModule state'])

           -- create commandline for this tempfile
           libpathString <- concatPaths (libraryDirs state') "."
           (Right command) <- commandline "helium" ["-P", libpathString, tempfile] (binaryDirs state')

           -- execute helium.
           -- interpreter will be executed when this process is finished.
           (sendF,process,pid) <- processExecAsync window command 512
                                                   (onEndCompilerProcess tempfile (sequenceNr state'))
                                                   (\s _ -> addInterpreterOutput interpreter (sequenceNr state') False s)
                                                   (\s _ -> addInterpreterOutput interpreter (sequenceNr state') True s)

           -- register the process and processid so that we are able to kill
           -- the interpreter if neccesairy.
           let state'' = state' { running     = Just (sendF, process, pid)
                                , alreadyDead = False
                                , tempFiles   = tempFiles state' ++ [tempfile]
                                }
           varSet interpreter state''

           return ()

  where
    -- write the main module
    writeMainModule :: FilePath -> String -> [String] -> IO ()
    writeMainModule path expression modules
      = do fd <- openFile path WriteMode
           hPutStrLn fd ("module " ++ interpreterMainModule ++ " where")
           hPutStrLn fd ""
           mapM_ (hPutStrLn fd . ("import " ++)) modules -- imports
           hPutStrLn fd ""
           hPutStrLn fd "interpreter_main ="
           mapM_ (hPutStrLn fd . ("  " ++)) (lines expression) -- prefix multi line expressions
           hFlush fd
           hClose fd
    
    escape :: String -> String
    escape str
      = "\"" ++ concatMap (\c -> case c of
                                   '\\' -> "\\\\"
                                   x    -> [x]
                          ) str ++ "\""


    -- ensure that the compile process has been completed and execute lvmrun.
    onEndCompilerProcess :: String -> Int -> Int -> IO ()
    onEndCompilerProcess sourcefile seqNr _
      = do state <- varGet interpreter
           when (seqNr == sequenceNr state)
                ( do -- Aborted: dump any remaining output and remove temp-files
                     let aborted = evaluationAborted state
                     when ( aborted )
                          ( do addInterpreterOutput interpreter seqNr False ""
                               removeTempFiles interpreter seqNr
                               varSet interpreter state { running = Nothing, alreadyDead = True }
                               reset interpreter
                          )

                     -- parse the results to investigate errors
                     when ( not aborted )
                          ( do input <- pendingOutput interpreter
                               let (modules, strangeOutput) = O.parseHeliumOutput input

                               -- figure out the last successfully compiled module.
                               let okCompiledModules = filter ( \m -> case (O.result m) of
                                                                        O.FinishedOk             -> True
                                                                        O.FinishedWithWarnings _ -> True
                                                                        _                        -> False
                                                              ) modules
                               let lastOkCompiledModule = if null okCompiledModules
                                                          then "Prelude"
                                                          else if length okCompiledModules > 1
                                                               then O.name (head $ drop 1 $ reverse okCompiledModules)
                                                               else O.name (last okCompiledModules)
                               onlyModuleName <- getNameOnly $ lastOkCompiledModule
                               let currentModulePath = if onlyModuleName == interpreterMainModule
                                                       then "Prelude"
                                                       else lastOkCompiledModule

                               varSet interpreter state { reversedPendingOutput = []
                                                        , alreadyDead = True
                                                        , compilationCompleted = True
                                                        , currentModule = currentModulePath
                                                        }



                               -- publish results
                               publishCompilationResults interpreter modules strangeOutput

                               let noInternalError = null strangeOutput
                               let compilationOk = all ( \m -> case O.result m of
                                                                 O.FinishedOk             -> True
                                                                 O.FinishedWithWarnings _ -> True
                                                                 _                        -> False
                                                       ) modules
                               let proceedWithLVM = noInternalError && compilationOk && not (compileOnly state)

                               when ( proceedWithLVM )
                                    ( executeLVM sourcefile )

                               when ( not $ proceedWithLVM )
                                    ( do reset interpreter
                                         finishCallback state interpreter
                                    )
                          )
                )
           return ()


    -- executes the lvm. Supply the initial source file (not the module file)
    executeLVM :: String -> IO ()
    executeLVM sourceModule
      = do let lvmModule = replaceSuffix ".hs" ".lvm" sourceModule
           state <- varGet interpreter

           -- create commandline for this lvm file
           libpathString <- concatPaths (libraryDirs state) "."
           (Right command) <- commandline "lvmrun" ["-P" ++ libpathString, lvmModule] (binaryDirs state)

           -- execute lvmrun.
           (sendF,process,pid) <- processExecAsync window command 64
                                                   (onEndLvmProcess (sequenceNr state))
                                                   (\s _ -> addInterpreterOutput interpreter (sequenceNr state) False s)
                                                   (\s _ -> addInterpreterOutput interpreter (sequenceNr state) True s)

           -- register the process and processid so that we are able to kill
           -- the interpreter if neccesary.
           varSet interpreter state { running     = Just (sendF, process, pid)
                                    , tempFiles   = tempFiles state ++ [lvmModule]
                                    , alreadyDead = False
                                    }


    -- called when lvm is terminated.
    onEndLvmProcess :: Int -> Int -> IO ()
    onEndLvmProcess seqNr _
      = do state <- varGet interpreter
           when ( seqNr == sequenceNr state )
                ( do removeTempFiles interpreter seqNr
                     varSet interpreter state { running = Nothing, alreadyDead = True }
                     reset interpreter
                     finishCallback state interpreter
                )


    -- output the results
    publishCompilationResults :: Interpreter -> [O.Module] -> String -> IO ()
    publishCompilationResults interpreter modules strangeOutput
      = do state <- varGet interpreter
           let allOk = all ( \m -> case (O.result m) of
                                     O.FinishedOk             -> True
                                     O.FinishedWithWarnings _ -> True
                                     _                        -> False
                           ) modules

           -- Publish the compilation results of individual modules.
           mapM_ (publishModule interpreter allOk) modules

           -- dumps any strange output as error on the screen.
           -- strange output is the result of unexpected output
           -- from helium (possibly an internal error of some kind).
           dataAvailableCallback state interpreter $ ErrorOutput strangeOutput Nothing

           -- dump the type of the expression if required.
           when (showExprType state)
             $ do -- look it up first
                  let mainModules = filter (\m -> interpreterMainModule `isSubsequenceOf` O.name m) modules
                  let warnings = [ s
                                 | m <- mainModules
                                 , n <- O.notifications m
                                 , s <- case n of
                                          O.Warning _ s -> [s]
                                          _             -> []
                                 ]
                  let linestart = "Warning: Missing type signature: interpreter_main :: "
                  let signatures = [ drop (length linestart) l
                                   | l <- warnings
                                   , take (length linestart) l == linestart
                                   ]
                  when (not (null signatures))
                    $ dataAvailableCallback state interpreter $ WarningOutput (expression ++ " :: " ++ head signatures) Nothing

                  return ()

           return ()

    -- output the results of one module
    publishModule :: Interpreter -> Bool -> O.Module -> IO ()
    publishModule interpreter allOk m
      = do state <- varGet interpreter
           onlyModuleName <- getNameOnly $ O.name m
           let isMainModule = onlyModuleName == interpreterMainModule
           let publish = dataAvailableCallback state interpreter
           ( case (O.result m) of
               O.FinishedOk             -> when (not isMainModule && not allOk && onlyModuleName /= "Prelude")
                                             $ publish $ NormalOutput ("Compiled " ++ onlyModuleName ++ "\n")
               O.FinishedWithWarnings n -> do when (not isMainModule)
                                                $ publish $ WarningOutput ("Compiling " ++ onlyModuleName ++ "\n") Nothing
                                              mapM_ (publishNotification interpreter isMainModule (O.name m)) (O.notifications m)
                                              when (not isMainModule)
                                                $ publish $ WarningOutput ("Successfully compiled " ++ onlyModuleName ++ " with " ++ (show n) ++ " warning" ++ (if n == 1 then "" else "s") ++ "\n") Nothing
               O.FinishedWithErrors n   -> do when (not isMainModule)
                                                $ publish $ ErrorOutput ("Compiling " ++ onlyModuleName ++ "\n") Nothing
                                              mapM_ (publishNotification interpreter isMainModule (O.name m)) (O.notifications m)
                                              when (not isMainModule)
                                                $ publish $ ErrorOutput ("Failed compiling " ++ onlyModuleName ++ " with " ++ (show n) ++ " error" ++ (if n == 1 then "" else "s") ++ "\n") Nothing
               O.NotFinished            -> do when isMainModule
                                                $ publish $ ErrorOutput ("Compiling of the expression did not finish!\n") Nothing
                                              when (not isMainModule)
                                                $ publish $ ErrorOutput ("Compiling of " ++ onlyModuleName ++ " did not finish!\n") Nothing
            )

    -- output the result of a notification
    publishNotification :: Interpreter -> Bool -> String -> O.Notification -> IO ()
    publishNotification interpreter isMainModule modulename notification
      = do state <- varGet interpreter
           let publish str lnk = dataAvailableCallback state interpreter $ case notification of
                                                                             O.Error   _ _ -> ErrorOutput str lnk
                                                                             O.Warning _ _ -> WarningOutput str lnk
           let (locations, text, isWarning) = case notification of
                                                O.Error l s   -> (l, s, False)
                                                O.Warning l s -> (l, s, True)

           -- publish the location of the notification
           when (not isMainModule && null locations && not (isWarning && ignoreWarnings state))
             $ do foldl1 (\ioL ioR -> do ioL; publish ", " Nothing; ioR) (map (\(r,c) -> publishLink publish r c) locations)
                  publish ": " Nothing

           -- publish the notification text
           when (not (isMainModule && isWarning) && not (isWarning && ignoreWarnings state))
             $ publish text Nothing
      where
        publishLink :: (String -> Maybe InFileLink -> IO ()) -> Int -> Int -> IO ()
        publishLink pf r c
          = let txt = "(" ++ show r ++ "," ++ show c ++ ")"
             in pf txt (Just (modulename, r, c))


-- attempts to load the given module.
loadModule :: Window a -> Interpreter -> String -> IO ()
loadModule window interpreter mod
  = do state <- varGet interpreter
       when (not $ isJust $ running state)
         $ do let state' = state { targetModule = mod }
              varSet interpreter state'
              evaluate window interpreter True True False "\"should not see this :)\""


-- adds the module to the also-load-modules list.
-- loading of this module is always attempted, unless it is unloaded.
-- As of yet: loading of the module is not checked at load time.
alsoLoadModule :: Interpreter -> String -> IO ()
alsoLoadModule interpreter mod
  = do state <- varGet interpreter
       when (not $ isJust $ running state)
         $ do let state' = state { additionalModules = additionalModules state ++ [mod] }
              varSet interpreter state'


-- unloads this module from the also-load-modules list.
-- it does not unload the non-also-modules.
unloadModule :: Interpreter -> String -> IO ()
unloadModule interpreter mod
  = do state <- varGet interpreter
       when (not $ isJust $ running state)
         $ do let state' = state { additionalModules = filter (/= mod) $ additionalModules state }
              varSet interpreter state'


-- compile the given expression to retrieve its type
compileForType :: Window a -> Interpreter -> String -> IO ()
compileForType window interpreter expr
  = do state <- varGet interpreter
       when (not $ isJust $ running state)
         $ evaluate window interpreter True True True expr


-- remove temporary files created while compiling.
removeTempFiles :: Interpreter -> Int -> IO ()
removeTempFiles interpreter seqNr
  = do state <- varGet interpreter
       when ( seqNr == sequenceNr state )
            ( mapM_ removeTempFile (tempFiles state) )
  where
    removeTempFile :: FilePath -> IO ()
    removeTempFile
      = unitIO . try . removeFile


-- add output to the interpreter.
addInterpreterOutput :: Interpreter -> Int -> Bool -> String -> IO ()
addInterpreterOutput interpreter seqNr isError text
  = do state <- varGet interpreter
       when (seqNr == sequenceNr state)
            ( do -- Aborted: dump any remaining output as error
                 when ( evaluationAborted state )
                      ( do pending <- pendingOutput interpreter
                           dataAvailableCallback state interpreter (ErrorOutput (pending ++ text) Nothing)
                           varSet interpreter state { reversedPendingOutput = [] }
                           return ()
                      )

                 -- Still compiling.
                 when ( not(evaluationAborted state) && not(compilationCompleted state) )
                      ( unitIO $ varSet interpreter state { reversedPendingOutput = text : reversedPendingOutput state } )

                 -- Finished compiling. Just dump it, either as normal or error text.
                 when ( not(evaluationAborted state) && compilationCompleted state )
                      ( dataAvailableCallback state interpreter $ toOutput isError text )
            )
       return ()
  where
    toOutput :: Bool -> String -> InterpreterOutput
    toOutput True  = flip ErrorOutput Nothing
    toOutput False = NormalOutput


-- Returns the pending output of the interpreter.
pendingOutput :: Interpreter -> IO String
pendingOutput interpreter
  = do state <- varGet interpreter
       return $ concat $ reverse $ reversedPendingOutput state


-- Resets the interpreter to idle state, killing any possible running
-- interpreter. Calling reset does not and will prevent the invocation
-- of the finish handler for this evaluation.
reset :: Interpreter -> IO ()
reset interpreter
  = do state <- varGet interpreter
       maybe ( return () )
             ( \(_, _, pid) ->
                 do when (not $ alreadyDead state)
                      $ unitIO $ kill pid wxSIGKILL
                    removeTempFiles interpreter (sequenceNr state)
             )
             ( running state )

       varSet interpreter state { compilationCompleted = False
                                , evaluationAborted    = True
                                , running              = Nothing
                                , alreadyDead          = True
                                }


-- Sends text to currently running process.
send :: Interpreter -> String -> IO ()
send interpreter input
  = do state <- varGet interpreter
       when ( not $ evaluationAborted state && isJust (running state) )
            ( let Just (sendF, _, _) = running state
               in sendF input
            )

