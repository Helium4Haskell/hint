-- Still working on it

module HeliumOutputParser
where



import Data.Char
import Data.Either
import Data.List
import LineParser
import StringOps



data Module
  = Module { name          :: String
           , notifications :: [Notification]
           , result        :: ModuleResult
           , scope         :: [(String, String)]
           }
  deriving Show


data Notification
  = Error   [(Int, Int)] String
  | Warning [(Int, Int)] String
  deriving Show


data ModuleResult
  = FinishedOk
  | FinishedWithWarnings Int
  | FinishedWithErrors Int
  | NotFinished
  deriving Show


data IntermediateResult
  = StartCompiling String
  | Notification Notification
  | EndCompiling ModuleResult
  | UnkownOutput String
  deriving Show



-- | Parses the output of the helium compiler
--   Returns the list of modules whose output is
--   successfully parsed and a string of unrecognized
--   output.
pHeliumOutput :: LineParser IntermediateResult ([Module], String)
pHeliumOutput
  = do modules <- many pModule
       unkown  <- many ( do s <- pUnkown
                            addInterResult (UnkownOutput s)
                            return s
                       )
       return (modules, unlines unkown)


-- | Parses the compilation of a module.
pModule :: LineParser IntermediateResult Module
pModule
  = do name <- pStartCompiling
       addInterResult (StartCompiling name)
       scope <- pScope
       many pEmptyLine
       notifications <- many ( do notification <- pNotification
                                  many pEmptyLine
                                  return notification
                             )
       result <-  (  pEndCompiling
                 <|> return NotFinished
                  )
       addInterResult (EndCompiling result)
       many pEmptyLine
       return $ Module { name          = name
                       , notifications = notifications
                       , result        = result
                       , scope         = scope
                       }
 <|> do mod <- pUpToDate
        many pEmptyLine
        return mod


-- | Parses the up-to-date message.
pUpToDate :: LineParser IntermediateResult Module
pUpToDate
  = mkParser "pUpToDate"
             ( \input -> if "is up to date" `isSubsequenceOf` input
                         then Just $ Module { name          = head $ words input
                                            , notifications = []
                                            , result        = FinishedOk
                                            , scope         = []
                                            }
                         else Nothing
             )


-- | Parses the:
--     Compiling <module name>
--   output of helium and returns the name of the module.
pStartCompiling :: LineParser IntermediateResult String
pStartCompiling
  = mkParser "pCompiling"
             ( \input -> let w = words input
                          in if length w >= 2 && "compiling" `isPrefixOf` (toLowerCase $ head w)
                             then Just $ unwords $ tail $ w
                             else Nothing
             )


-- | Parses the end sentence of compiling one module
pEndCompiling :: LineParser IntermediateResult ModuleResult
pEndCompiling
  =  mkParser "pEndCompiling_error"
              ( \input -> if sFailed `isPrefixOf` input
                          then Just $ FinishedWithErrors $ read $ takeWhile isDigit $ drop (length sFailed) input
                          else Nothing
              )
 <|> mkParser "pEndCompiling_warning"
              ( \input -> if sWarning `isPrefixOf` input
                          then Just $ FinishedWithWarnings $ read $ takeWhile isDigit $ drop (length sWarning) input
                          else Nothing
              )
 <|> mkParser "pEndCompiling_ok"
              ( \input -> if sOk `isPrefixOf` input
                          then Just $ FinishedOk
                          else Nothing
              )
  where sFailed  = "Compilation failed with "
        sWarning = "Compilation successful with "
        sOk      = "Compilation successful"


-- | Parses a notification. A notification is a warning or error from
--   the helium compiler. If the first line of the notification consists
--   of the word 'warning:', then the notification is considered a
--   warning, otherwise an error.
pNotification :: LineParser IntermediateResult Notification
pNotification
  =  do (locations, message) <- pFirstNotificationLine
        additional <- many pNotificationLine
        let notification = ( if "warning:" `isSubsequenceOf` (toLowerCase message)
                             then Warning
                             else Error
                           ) locations (unlines $ message : additional)
        addInterResult (Notification notification)
        return notification


-- | Parses the first line of the notification.
--
--   This is not a very nice function... unfortunately, the
--   regex library is not available for ghc 6.0.2 on win xp.
--   I could use Parsec, but it seems a little overkill...
pFirstNotificationLine :: LineParser IntermediateResult ([(Int, Int)], String)
pFirstNotificationLine
  = mkParser "pFirstNotificationLine"
             ( \input -> if length input > 0 && head input == '('
                         then do (sPositions, message) <- hasColumn input
                                 hasChar ',' sPositions
                                 hasChar '(' sPositions
                                 hasChar ')' sPositions
                                 positions <- isPositions $ getPositions $ sPositions
                                 return (positions, message)
                         else Nothing
             )
  where
    hasColumn :: String -> Maybe (String, String)
    hasColumn input
      = let result@(l, r) = break (== ':') input
         in if null r
            then Nothing
            else Just result

    hasChar :: Char -> String -> Maybe ()
    hasChar char input
      | char `elem` input = Just ()
      | otherwise         = Nothing

    getPositions :: String -> [(String, String)]
    getPositions input
      = let l = split '(' input
            l' = map (split ',') l
            f (x:y:_) = (x, takeWhile (/= ')') y)
            f _       = ("", "")
         in map f l'
    
    isPositions :: [(String, String)] -> Maybe [(Int, Int)]
    isPositions
      = foldr ( \(l,r) n -> do x <- toIntTuple l r
                               xs <- n
                               return (x : xs)
              ) (return [])
    
    toIntTuple :: String -> String -> Maybe (Int, Int)
    toIntTuple l r
      | all isDigit l && all isDigit r = Just (read l, read r)
      | otherwise = Nothing


-- | Parses a line in a notification.
pNotificationLine :: LineParser interResult String
pNotificationLine
  = mkParser "pBlockLine"
             ( \input -> if " " `isPrefixOf` input
                         then Just input
                         else Nothing
             )


-- | Parses the scope information of a helium module.
pScope :: LineParser interResult [(String, String)]
pScope
  = do pStartsWith "Fixity declarations:"
       many pNotificationLine
       pStartsWith "Data types:"
       many pNotificationLine
       pStartsWith "Type synonyms:"
       many pNotificationLine
       pStartsWith "Value constructors:"
       constructorTypes <- many pTypeDeclaration
       pStartsWith "Functions:"
       functionTypes <- many pTypeDeclaration
       return (constructorTypes ++ functionTypes)
 <|> return []


-- | Parses a type declaration.
pTypeDeclaration :: LineParser interResult (String, String)
pTypeDeclaration
  = mkParser "pTypeDeclaration"
             ( \input ->
                 let ws = words input
                     (n:s:ts) = ws
                  in if length ws >= 3 && s == "::"
                     then Just (n, unwords ts)
                     else Nothing
             )


-- | Parses an unkown sentence.
pUnkown :: LineParser interResult String
pUnkown
  = mkParser "pUnkown" Just


-- | Parses a sentence that has the given prefix (ignores case).
pStartsWith :: String -> LineParser interResult String
pStartsWith prefix
  = mkParser ( "pStartsWith_" ++ prefix )
             ( \input -> if toLowerCase prefix `isPrefixOf` toLowerCase input
                         then Just input
                         else Nothing
             )


-- | Parses a single empty line (a line consisting only of spaces)
pEmptyLine :: LineParser interResult ()
pEmptyLine
  = mkParser "pEmptyLine"
             ( \input -> if all isSpace input
                         then Just ()
                         else Nothing
             )

