module CommandlineOpts (getCommandlineArgs)
where


import System
import System.Console.GetOpt


data Flag
  = HeliumPath  FilePath
  | LvmrunPath  FilePath
  | EditorPath  FilePath
  deriving Show


options :: [OptDescr Flag]
options
  = [ Option ['L'] ["lvmrun"] (ReqArg LvmrunPath  "PATH")   "path to lvmrun executable"
    , Option ['H'] ["helium"] (ReqArg HeliumPath  "PATH")   "path to helium executable"
    , Option ['E'] ["editor"] (ReqArg EditorPath  "PATH") "path to editor executable (template patterns: %f %c %r)"
    ]


-- | Returns the installation directory provided as commandline parameter.
getCommandlineArgs :: IO (FilePath, FilePath,  FilePath)
getCommandlineArgs
  = do args <- getArgs
       let results = getOpt Permute options args
       process results
  where
    process (r, _, []) = do h <- retrieve "--helium PATH" isOptionH r
                            l <- retrieve "--lvmrun PATH" isOptionL r
                            e <- retrieve "--editor PATH" isOptionE r
                            return (h, l, e)
    process (_,_,errs) = fail ("Invalid commandline parameters:\n" ++ concat errs ++ usageInfo "usage: Hint [OPTION...]" options)

    retrieve :: String -> (Flag -> Bool) -> [Flag] -> IO String
    retrieve paramName pred flags = case (dropWhile (not . pred) flags) of
                                     (s:_) -> return $ getPath s
                                     _     -> fail ("unspecified commandline parameter: " ++ paramName ++ ".")

    isOptionH :: Flag -> Bool
    isOptionH (HeliumPath _) = True
    isOptionH _              = False

    isOptionL :: Flag -> Bool
    isOptionL (LvmrunPath _) = True
    isOptionL _              = False

    isOptionE :: Flag -> Bool
    isOptionE (EditorPath _) = True
    isOptionE _              = False

    getPath :: Flag -> String
    getPath (HeliumPath s) = s
    getPath (LvmrunPath s) = s
    getPath (EditorPath s) = s

