DoesNotExists.hs is up to date


Compiling Corrupted.hs
Fixity declarations:
   infixl 9 !!
   infixr 9 .
   infixr 8 **., ^, ^.
   infixl 7 *, *., /., `div`, `mod`, `quot`, `rem`
   infixl 6 +, +., -, -., `floatUnaryMinus`, `negate`
   infixr 5 ++, :
   infix  4 /=, /=., <, <., <=, <=., ==, ==., >, >., >=, >=.
   infixr 3 &&
   infixr 2 ||
   infixr 0 $, $!
Data types:
   data ()
   data -> a b
   data [] a
   data Bool
   data Char
   data Either a b
   data Float
   data Handle
   data IO a
   data IOMode
   data Int
   data Maybe a
   data Ordering
Type synonyms:
   type String = [Char]
Value constructors:
   () :: ()
   (:) :: a -> [a] -> [a]
   [] :: [a]
   AppendMode :: IOMode
   EQ :: Ordering
   False :: Bool
   GT :: Ordering
   Just :: a -> Maybe a
   LT :: Ordering
   Left :: a -> Either a b
   Nothing :: Maybe a
   ReadMode :: IOMode
   Right :: a -> Either b a
   True :: Bool
   WriteMode :: IOMode
Functions:
   (!!) :: [a] -> Int -> a
   ($) :: (a -> b) -> a -> b
   ($!) :: (a -> b) -> a -> b
   (&&) :: Bool -> Bool -> Bool
   (*) :: Int -> Int -> Int
   (**.) :: Float -> Float -> Float
   (*.) :: Float -> Float -> Float
   (+) :: Int -> Int -> Int
   (++) :: [a] -> [a] -> [a]
   (+.) :: Float -> Float -> Float
   (-) :: Int -> Int -> Int
   (-.) :: Float -> Float -> Float
   (.) :: (a -> b) -> (c -> a) -> c -> b
   (/) :: Int -> Int -> Int
   (/.) :: Float -> Float -> Float
   (/=) :: Int -> Int -> Bool
   (/=.) :: Float -> Float -> Bool
   (<) :: Int -> Int -> Bool
   (<.) :: Float -> Float -> Bool
   (<=) :: Int -> Int -> Bool
   (<=.) :: Float -> Float -> Bool
   (==) :: Int -> Int -> Bool
   (==.) :: Float -> Float -> Bool
   (>) :: Int -> Int -> Bool
   (>.) :: Float -> Float -> Bool
   (>=) :: Int -> Int -> Bool
   (>=.) :: Float -> Float -> Bool
   (>>=) :: IO a -> (a -> IO b) -> IO b
   (^) :: Int -> Int -> Int
   (^.) :: Float -> Int -> Float
   (||) :: Bool -> Bool -> Bool
   abs :: Int -> Int
   absFloat :: Float -> Float
   all :: (a -> Bool) -> [a] -> Bool
   and :: [Bool] -> Bool
   any :: (a -> Bool) -> [a] -> Bool
   bracketIO :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
   break :: (a -> Bool) -> [a] -> ([a], [a])
   catch :: IO a -> (Exception -> IO a) -> IO a
   catchEof :: IO a -> IO a -> IO a
   ceiling :: Float -> Int
   chr :: Int -> Char
   concat :: [[a]] -> [a]
   concatMap :: (a -> [b]) -> [a] -> [b]
   const :: a -> b -> a
   cos :: Float -> Float
   curry :: ((a, b) -> c) -> a -> b -> c
   cycle :: [a] -> [a]
   div :: Int -> Int -> Int
   drop :: Int -> [a] -> [a]
   dropWhile :: (a -> Bool) -> [a] -> [a]
   either :: (a -> b) -> (c -> b) -> Either a c -> b
   elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
   eqBool :: Bool -> Bool -> Bool
   eqChar :: Char -> Char -> Bool
   eqFloat :: Float -> Float -> Bool
   eqInt :: Int -> Int -> Bool
   eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
   eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
   eqString :: String -> String -> Bool
   eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
   error :: String -> a
   even :: Int -> Bool
   exp :: Float -> Float
   filter :: (a -> Bool) -> [a] -> [a]
   finallyIO :: IO a -> IO b -> IO a
   fix :: (a -> a) -> a
   flip :: (a -> b -> c) -> b -> a -> c
   floor :: Float -> Int
   foldl :: (a -> b -> a) -> a -> [b] -> a
   foldl' :: (a -> b -> a) -> a -> [b] -> a
   foldl1 :: (a -> a -> a) -> [a] -> a
   foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr1 :: (a -> a -> a) -> [a] -> a
   fromInt :: Int -> Float
   fst :: (a, b) -> a
   gcd :: Int -> Int -> Int
   getChar :: IO Char
   getLine :: IO String
   hClose :: Handle -> IO ()
   hGetChar :: Handle -> IO Char
   hPutChar :: Handle -> Char -> IO ()
   hPutString :: Handle -> String -> IO ()
   head :: [a] -> a
   id :: a -> a
   init :: [a] -> [a]
   intToFloat :: Int -> Float
   isAlpha :: Char -> Bool
   isAlphaNum :: Char -> Bool
   isDigit :: Char -> Bool
   isLower :: Char -> Bool
   isSpace :: Char -> Bool
   isUpper :: Char -> Bool
   iterate :: (a -> a) -> a -> [a]
   last :: [a] -> a
   lcm :: Int -> Int -> Int
   length :: [a] -> Int
   lines :: String -> [String]
   log :: Float -> Float
   lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
   map :: (a -> b) -> [a] -> [b]
   max :: Int -> Int -> Int
   maximum :: [Int] -> Int
   maybe :: a -> (b -> a) -> Maybe b -> a
   min :: Int -> Int -> Int
   minimum :: [Int] -> Int
   mod :: Int -> Int -> Int
   negate :: Int -> Int
   not :: Bool -> Bool
   notElemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
   null :: [a] -> Bool
   odd :: Int -> Bool
   openFile :: String -> IOMode -> IO Handle
   or :: [Bool] -> Bool
   ord :: Char -> Int
   ordChar :: Char -> Char -> Ordering
   ordFloat :: Float -> Float -> Ordering
   ordInt :: Int -> Int -> Ordering
   ordList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
   ordString :: String -> String -> Ordering
   otherwise :: Bool
   pi :: Float
   primBindIO :: IO a -> (a -> IO b) -> IO b
   primConcat :: [[a]] -> [a]
   primConcatMap :: (a -> [b]) -> [a] -> [b]
   primEnumFrom :: Int -> [Int]
   primEnumFromThen :: Int -> Int -> [Int]
   primEnumFromThenTo :: Int -> Int -> Int -> [Int]
   primEnumFromTo :: Int -> Int -> [Int]
   primEqFloat :: Float -> Float -> Bool
   primErrorPacked :: PackedString -> a
   primNegFloat :: Float -> Float
   primNegInt :: Int -> Int
   primPackedToString :: PackedString -> String
   primPatternFailPacked :: PackedString -> a
   primPutChar :: Char -> IO ()
   primPutStr :: String -> IO ()
   primPutStrLn :: String -> IO ()
   primStringToFloat :: String -> Float
   primUnsafePerformIO :: IO a -> a
   print :: (a -> String) -> a -> IO ()
   product :: [Int] -> Int
   putChar :: Char -> IO ()
   putStr :: String -> IO ()
   putStrLn :: String -> IO ()
   quot :: Int -> Int -> Int
   raise :: Exception -> a
   readFile :: String -> IO String
   readInt :: [Char] -> Int
   readUnsigned :: String -> Int
   rem :: Int -> Int -> Int
   repeat :: a -> [a]
   replicate :: Int -> a -> [a]
   return :: a -> IO a
   reverse :: [a] -> [a]
   round :: Float -> Int
   scanl :: (a -> b -> a) -> a -> [b] -> [a]
   scanl1 :: (a -> a -> a) -> [a] -> [a]
   scanr :: (a -> b -> b) -> b -> [a] -> [b]
   scanr1 :: (a -> a -> a) -> [a] -> [a]
   seq :: a -> b -> b
   sequence :: [IO a] -> IO [a]
   sequence_ :: [IO a] -> IO ()
   showBool :: Bool -> String
   showChar :: Char -> String
   showEither :: (a -> String) -> (b -> String) -> Either a b -> String
   showFloat :: Float -> String
   showFunction :: (a -> String) -> (b -> String) -> (a -> b) -> String
   showIO :: (a -> String) -> IO a -> String
   showInt :: Int -> String
   showList :: (a -> String) -> [a] -> String
   showMaybe :: (a -> String) -> Maybe a -> String
   showOrdering :: Ordering -> String
   showPolymorphic :: a -> String
   showString :: String -> String
   showTuple10 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (f -> String) -> (g -> String) -> (h -> String) -> (i -> String) -> (j -> String) -> (a, b, c, d, e, f, g, h, i, j) -> String
   showTuple2 :: (a -> String) -> (b -> String) -> (a, b) -> String
   showTuple3 :: (a -> String) -> (b -> String) -> (c -> String) -> (a, b, c) -> String
   showTuple4 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (a, b, c, d) -> String
   showTuple5 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (a, b, c, d, e) -> String
   showTuple6 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (f -> String) -> (a, b, c, d, e, f) -> String
   showTuple7 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (f -> String) -> (g -> String) -> (a, b, c, d, e, f, g) -> String
   showTuple8 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (f -> String) -> (g -> String) -> (h -> String) -> (a, b, c, d, e, f, g, h) -> String
   showTuple9 :: (a -> String) -> (b -> String) -> (c -> String) -> (d -> String) -> (e -> String) -> (f -> String) -> (g -> String) -> (h -> String) -> (i -> String) -> (a, b, c, d, e, f, g, h, i) -> String
   showUnit :: () -> String
   signum :: Int -> Int
   signumFloat :: Float -> Int
   sin :: Float -> Float
   snd :: (a, b) -> b
   span :: (a -> Bool) -> [a] -> ([a], [a])
   splitAt :: Int -> [a] -> ([a], [a])
   sqrt :: Float -> Float
   stderr :: Handle
   stdin :: Handle
   stdout :: Handle
   subtract :: Int -> Int -> Int
   sum :: [Int] -> Int
   sumFloat :: [Float] -> Float
   tail :: [a] -> [a]
   take :: Int -> [a] -> [a]
   takeWhile :: (a -> Bool) -> [a] -> [a]
   tan :: Float -> Float
   toLower :: Char -> Char
   toUpper :: Char -> Char
   truncate :: Float -> Int
   uncurry :: (a -> b -> c) -> (a, b) -> c
   undefined :: a
   unlines :: [String] -> [Char]
   unsafePerformIO :: IO a -> a
   until :: (a -> Bool) -> (a -> a) -> a -> a
   unwords :: [String] -> [Char]
   unzip :: [(a, b)] -> ([a], [b])
   unzip3 :: [(a, b, c)] -> ([a], [b], [c])
   words :: String -> [String]
   writeFile :: String -> String -> IO ()
   x :: Int
   zip :: [a] -> [b] -> [(a, b)]
   zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
   zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
   zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

Compilation successful with 2 warnings


Compiling PartiallyOk.hs
(1,1): Warning: Missing type signature: x :: Int
(2,2),(4,5): A multi-line
  warning en nog wat meer
Compilation successful

Compiling Test2.hs
Compilation successful with 9999 warnings
Compiling PartiallyOk2.hs
(1,1): Warning: Missing type signature: x :: Int
(2,2),(4,5): A multi-line
  warning en nog wat meer
Compilation successful


Compiling Corrupted.hs
(4,4): Warning: Missing type signature: x :: Int
(5,XXXX): Warning: Missing type signature: y :: Int
Compilation successzful with 2 warnings

