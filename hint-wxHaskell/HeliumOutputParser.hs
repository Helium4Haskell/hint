module HeliumOutputParser
  ( parseHeliumOutput
  , Module (Module, name, notifications, result, scope)
  , Notification (Error, Warning)
  , ModuleResult (FinishedOk, FinishedWithWarnings, FinishedWithErrors, NotFinished)
  )
where



import Data.Char
import Data.Either
import Data.List
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
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


-- interface to the parser.
-- returns the list of modules outputed by helium and
-- a string with unrecognized output.
parseHeliumOutput :: String -> ([Module], String)
parseHeliumOutput input
  = let result = parse pHeliumOutput "Helium output" input
     in case result of
          Left err -> error ("(!!!) Unexpected result from Helium:\n" ++ show err)
          Right x -> x


-- | Parses the output of the helium compiler
--   Returns the list of modules whose output is
--   successfully parsed and a string of unrecognized
--   output.
pHeliumOutput :: Parser ([Module], String)
pHeliumOutput
  = do modules <- many $ try $ pModule
       unkown <- many $ anyChar
       return (modules, unkown)


-- | Parses the compilation of a module.
pModule :: Parser Module
pModule
  =  try pUncompiledModule
 <|> pCompiledModule
  where
    pUncompiledModule :: Parser Module
    pUncompiledModule
      = do name <- pStartCompiling
           scope <- pScope
           spaces
           notifications <- many $ try $ do notification <- pNotification
                                            spaces
                                            return notification
           result <-  (  try $ pEndCompiling
                     <|> return NotFinished
                      )
           spaces
           return $ Module { name = name
                           , notifications = notifications
                           , result = result
                           , scope = scope
                           }

    pCompiledModule :: Parser Module
    pCompiledModule
      = do mod <- pUpToDate
           spaces
           return mod


-- | Parses the:
--     Compiling <module name>
--   output of helium and returns the name of the module.
pStartCompiling :: Parser String
pStartCompiling
  = do string "Compiling "
       name <- pNonemptyLine
       return name


-- | Parses the scope information of a helium module.
pScope :: Parser [(String, String)]
pScope
  =  try ( do string "Fixity declarations:"
              pLine
              many pNonemptyLine
              string "Data types:"
              pLine
              many pNonemptyLine
              string "Type synonyms:"
              pLine
              many pNonemptyLine
              string "Value constructors:"
              pLine
              constructorTypes <- try $ many pTypeDeclaration
              spaces
              string "Functions:"
              pLine
              functionTypes <- try $ many pTypeDeclaration
              spaces
              return (constructorTypes ++ functionTypes)
         )
 <|> return []


-- | Parses a type declaration.
pTypeDeclaration :: Parser (String, String)
pTypeDeclaration
  = do spaces
       name <- manyTill anyChar (string "::")
       tp <- pNonemptyLine
       return (trimSpaces name, trimSpaces tp)


-- | Parses a notification. A notification is a warning or error from
--   the helium compiler. If the first line of the notification consists
--   of the word 'warning:', then the notification is considered a
--   warning, otherwise an error.
pNotification
  = do (locations, message) <- pFirstNotificationLine
       additional <- many pNotificationLine
       let notification = ( if "warning:" `isSubsequenceOf` (toLowerCase message)
                            then Warning
                            else Error
                           ) locations (unlines $ message : additional)
       return notification


-- | Parses the first line of the notification.
pFirstNotificationLine :: Parser ([(Int, Int)], String)
pFirstNotificationLine
  = do positions <- sepBy1 pTuple (char ',')
       string ": "
       reason <- pNonemptyLine
       return (positions, reason)
  where
    pTuple :: Parser (Int, Int)
    pTuple
      = do char '('
           row <- pInt
           char ','
           col <- pInt
           char ')'
           return (row, col)


-- Parses a line in a notification. This is a non-empty line.
pNotificationLine :: Parser String
pNotificationLine
  = do sps <- many1 (char ' ')
       txt <- pNonemptyLine
       return (sps ++ txt)


-- | Parses the end sentence of compiling one module
pEndCompiling :: Parser ModuleResult
pEndCompiling
  =  try pCompilingError
 <|> try pCompilingWarning
 <|> pCompilingOk
  where
    pCompilingError :: Parser ModuleResult
    pCompilingError
      = do string "Compilation failed with "
           errors <- pInt
           pLine
           return $ FinishedWithErrors errors

    pCompilingWarning :: Parser ModuleResult
    pCompilingWarning
      = do string "Compilation successful with "
           warnings <- pInt
           pLine
           return $ FinishedWithWarnings warnings

    pCompilingOk :: Parser ModuleResult
    pCompilingOk
      = do string "Compilation successful"
           newline
           return FinishedOk


-- | Parses the up-to-date message.
pUpToDate
  = do line <- pLine
       ( if "is up to date" `isSubsequenceOf` line
         then return $ Module { name       = head $ words line
                              , notifications = []
                              , result        = FinishedOk
                              , scope         = []
                              }
         else fail ("Is not an up-to-date message: " ++ line ++ ".")
        )


-- parses one single line of text.
pLine
  =  ( try $ manyTill anyChar (newline <|> (do string "\r\n"; return '\n')) )
 <|> ( do str <- many1 anyChar
          eof
          return str
     )


-- parses a non-empty line of text.
pNonemptyLine
  =  do str <- many1 $ noneOf ['\n', '\r']
        (newline <|> (do string "\r\n"; return '\n'))
        return str


-- parses an integer (>= 0)
-- needs to fit in an Int
pInt :: Parser Int
pInt
  = do digits <- many1 digit
       return $ read digits

