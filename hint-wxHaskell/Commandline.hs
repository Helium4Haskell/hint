-- | Use this module to build commandline strings.
module Commandline (commandline)
where



import qualified Control.Exception as E
import Control.Monad
import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import System
import System.Directory
import System.Environment



-- | Searches the list of directories for the given command and builds the
--   commandline for this command (if it exists) with the given parameters.
--
--   For example:
--     commandline "helium" ["-P", ".;C:\\Program Files\\Helium\\Lib"] ["C:\\Program Files\\Helium\\Bin"]
--   Produces:
--     "C:\Program Files\Helium\Bin\helium.exe" "-P" ".;C:\Program Files\Helium\Lib"
commandline :: String -> [String] -> [FilePath] -> IO (Either String String)
commandline command parameters binaryDirectories
  = do isWinOS <- isWindowsOS
       paths <- catch (getEnv "Path") (const $ return [])
       let directories = binaryDirectories ++ split (pathSeparator isWinOS) paths
       mBinpath <- pathTo command directories
       return $ maybe (Left $ "Unable to find \'" ++ command ++ "\' in:\n" ++ unlines directories) Right
              $ do binpath <- mBinpath
                   return $ concat [ "\""
                                   , escape isWinOS binpath
                                   , "\""
                                   , concatMap (\param -> concat [" \"", escape isWinOS param, "\""]) parameters
                                   ]


-- | Split a list on a token.
split :: Eq a => a -> [a] -> [[a]]
split = split' []

split' :: Eq a => [a] -> a -> [a] -> [[a]]
split' acc _ [] = [acc]
split' acc c (x:xs)
  | x == c = acc : split' [] c xs
  | otherwise = split' (acc ++ [x]) c xs


-- | Escapes the string for use on the commandline.
escape :: Bool -> String -> String
escape isWinOS
  | not isWinOS = concatMap
                $ \c -> case c of
                          '\"' -> "\\\""
                          '\\' -> "\\\\"
                          _    -> [c]
  | otherwise = id


-- | This functions searches the given list of directories
--   for the given executable and returns the path to it.
--   The first match is used.
pathTo :: String -> [FilePath] -> IO (Maybe FilePath)
pathTo name directories
  = do isWinOS <- isWindowsOS
       results <- mapM (find (dirSeparator isWinOS) (addSuffixIf ".exe" isWinOS name)) directories
       return $ listToMaybe $ catMaybes results
  where
    find :: Char -> String -> FilePath -> IO (Maybe FilePath)
    find sep name directory
      = do let file = concatPath sep directory name
           fileOK <- isValidExecutable file
           return $ if fileOK then Just file else Nothing
      `catch` (const $ return Nothing)

    isValidExecutable :: String -> IO Bool
    isValidExecutable file
      = do exists <- doesFileExist file
           isDirectory <- doesDirectoryExist file
           permissions <- getPermissions file
           return $ exists && not isDirectory && executable permissions

    addSuffixIf :: String -> Bool -> String -> String
    addSuffixIf suffix condition name
      | condition && not (isSuffixOf (map toLower suffix) (map toLower name)) = name ++ suffix
      | otherwise = name


-- | Concatenates paths using the given separator.
concatPath :: Char -> FilePath -> FilePath -> FilePath
concatPath sep pathBegin pathEnd
  = stripTrailingSeparator sep pathBegin ++ [sep] ++ pathEnd
  where
    stripTrailingSeparator :: Char -> FilePath -> FilePath
    stripTrailingSeparator sep
      = reverse . dropWhile (== sep) . reverse


-- | Returns the character that separates paths.
--   For windows-based operating systems, this is the
--   ';' character, for unix-based operating system, this
--   is the ':' character.
getPathSeparator :: IO Char
getPathSeparator
  = do b <- isWindowsOS
       return $ pathSeparator b


-- | Returns the character that separates paths.
--   For windows-based operating systems, this is the
--   ';' character, for unix-based operating system, this
--   is the ':' character.
pathSeparator :: Bool -> Char
pathSeparator isWinOS
  | isWinOS   = ';'
  | otherwise = ':'


-- | Returns the character that separates directories.
--   For windows-based operating systems, this is the
--   '\\' character, for unix-based operating system, this
--   is the '//' character.
getDirectorySeparator :: IO Char
getDirectorySeparator
  = do b <- isWindowsOS
       return $ dirSeparator b


-- | Returns the character that separates directories.
--   For windows-based operating systems, this is the
--   '\\' character, for unix-based operating system, this
--   is the '//' character.
dirSeparator :: Bool -> Char
dirSeparator isWinOS
  | isWinOS   = '\\'
  | otherwise = '/'


-- | Checks if we are using a MS Windows-based operating system.
--   This function is used to determine the directory and path
--   seperator for the current operating system.
--
--   Note: is there no cleaner way to make this distinction?
isWindowsOS :: IO Bool
isWindowsOS
  =    envContains "OS" "windows"
  `or` envContains "SystemRoot" ":\\"
  `or` envContains "ProgramFiles" ":\\"
  `or` envContains "Path" ":\\"
  `or` envContains "PATHEXT" ".EXE"
  `or` directoryIsWindowsStyle
  where
    envContains :: String -> String -> IO Bool
    envContains name value
      = do v <- E.catch (getEnv name) (const $ return "")
           return $ isSubstr (map toLower value) (map toLower v)

    directoryIsWindowsStyle :: IO Bool
    directoryIsWindowsStyle
      = do dir <- getCurrentDirectory
           return $ isSubstr dir ":\\"

    isSubstr :: String -> String -> Bool
    isSubstr key
      = any (isPrefixOf key) . tails

    or :: IO Bool -> IO Bool -> IO Bool
    or p q
      = do b <- p
           if b then return b else q

