-- | Simple Line-based parser combinators (greedy with lookahead of a single line of text)
--
-- A Primitive parser parses a single line. The combiniators combine this results. The
-- data is applied to the parser line by line. The result can be obtained when all the
-- required lines have been applied. It is possible to add an intermediate result while
-- parsing. These intermediate results are colleced and returned after the application
-- of each line.
--
-- Note: these parsers will crash when the parser keeps accepting Nothing as input,
--       for example in: many (return 3)
--
module LineParser ( LineParser
                  , ParseResult
                  , ParseState
                  , (<|>)
                  , addInterResult
                  , mkParser
                  , startParsing
                  , addData
                  , endParsing
                  , many
                  )
where



import Control.Monad
import Data.Either
import Data.Maybe



-- | The parser. It consists of a boolean constant that specified whether or not it is
--   known that the parser a-priori consumes input (a parser that does not consume
--   input can always be applied) and the function that parses input. Errors are
--   returned as a string, unexpected input by a Nothing result. Errors propagate all
--   the way back, rejected input only if there's no alternative parser that accepts the
--   input.
data LineParser interResult finalResult
  = LineParser { consumes  :: Bool
               , parseLine :: [interResult] -> Maybe String -> Either String (Maybe (ParseResult interResult finalResult))
               }


-- | The result optained after a successful parsing of a line. It consists of a list
--   of intermediate results so far (since the beginning of the addData function),
--   a boolean that specified whether or not the input was consumed by this function,
--   and the actual result, which could be the final result, or a parser that will
--   give this result after applying a few more lines.
data ParseResult interResult finalResult
  = ParseResult { results  :: [interResult]
                , consumed :: Bool
                , result   :: Either (LineParser interResult finalResult) finalResult
                }


-- | Contains the state of the parsing process between applying lines.
data ParseState interResult finalResult
  = ParseState { remaining :: String
               , state     :: Either (LineParser interResult finalResult) finalResult
               }



-- | Allows us to use the do syntax for sequencing parsers and modifying results.
instance Monad (LineParser interResult) where
  -- | Applies the first parser and passes the result to the next.
  parser >>= nextParserF
    = parser { parseLine = \interResults input -> either Left (Right . processParseResults) (parseLine parser interResults input) }
    where
      processParseResults maybeParseResult
        = do pResult <- maybeParseResult
             return $ pResult { result = Left $ either (>>= nextParserF) (nextParserF) (result pResult) }


  -- | Creates a trivial parser.
  return result
    = LineParser { consumes  = False
                 , parseLine = \results _ -> Right $ Just $ ParseResult { results  = results
                                                                        , consumed = False
                                                                        , result   = Right result
                                                                        }
                 }


  -- | Fails the parse process and specifies an error message.
  fail msg
    = LineParser { consumes  = False
                 , parseLine = \_ _ -> Left msg
                 }



infixr 4 <|>

-- | The "alternative"-combinator. This combinator is greedy: the second parser is only
--   used if the first one fails for this line of input.
(<|>) :: LineParser interResult finalResult -> LineParser interResult finalResult -> LineParser interResult finalResult
fstParser <|> sndParser
  = LineParser { consumes  = any consumes [fstParser, sndParser]
               , parseLine = \interResults input ->
                               let tryLeftRight = either Left (maybe tryRight (Right . Just)) tryLeft
                                   tryLeft      = parseLine fstParser interResults input
                                   tryRight     = parseLine sndParser interResults input
                                in if isNothing input || (not $ any consumes [fstParser, sndParser])
                                   then if consumes fstParser
                                        then tryRight
                                        else tryLeft
                                   else tryLeftRight
               }


-- | Combinators that adds an intermediate result.
addInterResult :: interResult -> LineParser interResult ()
addInterResult interResult
  = LineParser { consumes  = False
               , parseLine = \results _ -> Right $ Just $ ParseResult { results  = interResult : results
                                                                      , consumed = False
                                                                      , result   = Right ()
                                                                      }
               }


-- | This function constructs primitive parsers. Specify name and primitive parse function.
mkParser :: String -> (String -> Maybe finalResult) -> LineParser interResult finalResult
mkParser name pf
  = LineParser { consumes  = True
               , parseLine = \results mInput -> case mInput of
                                                  Nothing    -> Left  $ "error: parser " ++ name ++ " does not accept end of input."
                                                  Just input -> Right $ do finalResult <- pf input
                                                                           return $ ParseResult { results  = results
                                                                                                , consumed = True
                                                                                                , result   = Right finalResult
                                                                                                }
               }


-- | Exhaustivly applies a parser to a line of input (that does not contain a line end).
--   Specify Nothing for end of input.
applyParser :: LineParser interResult finalResult -> [interResult] -> Maybe String -> Either String ([interResult], Either (LineParser interResult finalResult) finalResult)
applyParser parser interResults input
  | consumes parser
      = either Left (maybe (Left $ "applyParser: error: parser failed on: " ++ show input) processConsumer) (parseLine parser interResults input)
  | otherwise
      = either Left (maybe (Left "applyParser: error: a parser that not consumes input should always yield a result!") processNonConsumer) (parseLine parser interResults Nothing)
  where
    processConsumer pResult
      = case result pResult of
          Left nextParser -> if (not $ consumed pResult) || (not $ consumes nextParser)
                             then applyParser nextParser (results pResult) (if consumed pResult then Nothing else input)
                             else Right $ (results pResult, Left nextParser)
          Right result    -> if (not $ consumed pResult) && isJust input
                             then Left  $ "applyParser: error: there's still input remaining: " ++ fromJust input
                             else Right $ (results pResult, Right result)

    processNonConsumer pResult
      = case result pResult of
          Left  nextParser -> if consumes nextParser && (consumed pResult || isNothing input)
                              then Right $ (results pResult, Left nextParser)
                              else applyParser nextParser (results pResult) (if consumed pResult then Nothing else input)
          Right result     -> case input of
                                Just x  -> Left  $ "applyParser: error: there's still input remaining: " ++ x
                                Nothing -> Right $ (results pResult, Right result)



-- | Turns a parser into a parse state. This will start the parse process. Data can be added using
--   the addData function.
startParsing :: LineParser interResult finalResult -> ParseState interResult finalResult
startParsing parser
  = ParseState { remaining = ""
               , state     = Left parser
               }


-- | Adds data to the parser. It is not required to add full single lines to this function. A prefix of a
--   line or multiply lines at once are allowed. Lines that are not yet complete are stored untill the
--   full line is available.
addData :: String -> ParseState interResult finalResult -> Either String ([interResult], ParseState interResult finalResult)
addData input st
  = let inputs    = lines $ remaining st ++ input ++ "\n"
        inputs'   = if null inputs then [] else init inputs
        left      = if null inputs then "" else last inputs
     in foldl applyParserToLine (Right ([], st { remaining = left })) inputs'
  where
    applyParserToLine :: Either String ([interResult], ParseState interResult finalResult) -> String -> Either String ([interResult], ParseState interResult finalResult)
    applyParserToLine (Left msg) _
      = Left msg
    applyParserToLine passthrough@(Right (results, st)) input
      = case state st of
          Left p  -> case applyParser p results (Just input) of
                       Left msg                -> Left msg
                       Right (results, result) -> Right (results, st { state = result })
          Right _ -> if not $ null input
                     then Left  $ "addData: error: There's no parser in the parse state, only a result, but there's input remaining: " ++ input
                     else passthrough


-- | Ends the parsing process. An error message is returned when not all the data has been passed to
--   the parser (which means that there's no result yet).
endParsing :: ParseState interResult finalResult -> Either String ([interResult], finalResult)
endParsing parseState
  = let eParseState = if not $ null $ remaining parseState
                      then addData (remaining parseState) (parseState { remaining = "" })
                      else Right ([], parseState)
     in case eParseState of
          Left msg                     -> Left msg
          Right (results, parseState') -> case state parseState' of
                                            Right r -> Right (results, r)
                                            Left  p -> iterateNothing p results
  where
    iterateNothing :: LineParser interResult finalResult -> [interResult] -> Either String ([interResult], finalResult)
    iterateNothing parser results
      = case applyParser parser results Nothing of
          Left msg                            -> Left msg
          Right (newResults, Left nextParser) -> iterateNothing nextParser newResults   -- loops if it keeps accepting Nothing!
          Right (results, Right r)            -> Right (results, r)



-- | The many parser.
many :: LineParser b a -> LineParser b [a]
many p
  =  ( do x <- p
          xs <- many p
          return (x : xs)
     )
 <|> return []

