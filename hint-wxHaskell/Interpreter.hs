module Interpreter
where



import Data.Maybe
import IO
import Graphics.UI.WXCore
import Graphics.UI.WX
import System.Directory
import Path



type Interpreter = Var InterpreterState


data InterpreterState
  = IS { reversedPendingOutput :: [String]
       , compilationCompleted  :: Bool
       , evaluationAborted     :: Bool
       , running               :: Maybe (Process (), Int, [String])
       , sequenceNr            :: Int
       , dataAvailableCallback :: Interpreter -> InterpreterOutput -> IO ()
       , finishCallback        :: Interpreter -> IO ()
       , libraryDirs           :: [FilePath]
       , binaryDirs            :: [FilePath]
       }

data InterpreterOutput
  = NormalOutput String
  | WarningOutput String
  | ErrorOutput String



-- Creates an initial interpreter.
create :: (Interpreter -> InterpreterOutput -> IO ()) -> (Interpreter -> IO ()) -> [FilePath] -> [FilePath] -> IO Interpreter
create onOutput onFinish libDirs binDirs
  = varCreate IS { reversedPendingOutput = []
                 , compilationCompleted  = False
                 , evaluationAborted     = True
                 , running               = Nothing
                 , sequenceNr            = 0
                 , dataAvailableCallback = onOutput
                 , finishCallback        = onFinish
                 , libraryDirs           = libDirs
                 , binaryDirs            = binDirs
                 }


-- Runs the expression
evaluate :: Window a -> Interpreter -> String -> IO ()
evaluate window interpreter expression
  = let tempfileName = "Interpreter.hs"
     in do reset interpreter  -- abort any (possibly) running operations
           state <- varGet interpreter
           let state' = state { sequenceNr = sequenceNr state + 1 }

           -- build tempfile
           tempfile <- getTempFilename tempfileName
           writeMainModule tempfile expression

           -- create commandline for this tempfile
           libpathString <- concatPaths (libraryDirs state) "."
           putStrLn libpathString
           (Right command) <- commandline "helium" ["-P", libpathString, tempfile] (binaryDirs state)

           -- execute helium.
           (_,process,pid) <- processExecAsync window command 64
                                               (onEndProcess window)
                                               (\s _ -> addInterpreterOutput interpreter (sequenceNr state) False s)
                                               (\s _ -> addInterpreterOutput interpreter (sequenceNr state) True s)

           -- register lvmrun for evaluation.

           return ()
  where
    writeMainModule :: FilePath -> String -> IO ()
    writeMainModule path expression
      = writeFile path ( "module Interpreter where\r\ninterpreter_main = " ++ expression ++ "\r\n" )

    onEndProcess
      = undefined


-- Add output to the interpreter
addInterpreterOutput :: Interpreter -> Int -> Bool -> String -> IO ()
addInterpreterOutput interpreter seqNr isError text
  = do state <- varGet interpreter
       when (seqNr == sequenceNr state)
            ( do -- Aborted: dump any remaining output as error
                 when ( evaluationAborted state )
                      ( do pending <- pendingOutput interpreter
                           dataAvailableCallback state interpreter (ErrorOutput $ pending ++ text)
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
    toOutput True  = ErrorOutput
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
             ( \(_, pid, tempFiles) ->
                 do kill pid wxSIGKILL
                    mapM_ removeFile tempFiles
             )
             ( running state )
       varSet interpreter state { compilationCompleted = False
                                , evaluationAborted = True
                                , running = Nothing
                                }

