-- Open this file with hugs or ghci and run test.
--
-- If you run this file with hugs, make sure to increase the default heap size... :+

module HeliumOutputTest
where



import HeliumOutputParser
import LineParser
import System.IO



fromRight x
  = case x of
      Right y -> y
      Left  x -> error ("oops: " ++ x)

testParser
  = pHeliumOutput


test :: IO ()
test
  = do h <- openFile "helium-output1.tst" ReadMode
       let state = startParsing testParser
       endState <- nextLine h state
       putStrLn "No more data."
       let result = snd $ fromRight $ endParsing endState
       putStrLn ("final result: " ++ show result)
       hClose h


nextLine :: Show a => Handle -> ParseState a b -> IO (ParseState a b)
nextLine h initState
  = ( do line <- hGetLine h
         putStrLn ("Read line: " ++ line ++ ".")
         let (results, newstate) = fromRight $ addData (line ++ "\n") initState
         sequence_ $ map (\x -> putStrLn $ "res: " ++ show x) $ results
         nextLine h newstate
    ) `catch` (\_ -> return initState)

