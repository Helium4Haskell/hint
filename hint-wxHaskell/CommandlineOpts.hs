module CommandlineOpts (getInstallationDirectory)
where


import System
import System.Console.GetOpt



data Flag
  = InstallDir FilePath


options :: [OptDescr Flag]
options
  = [ Option ['P'] ["installdir"] (ReqArg InstallDir "DIR") "installation directory" ]


-- | Returns the installation directory provided as commandline parameter.
getInstallationDirectory :: IO FilePath
getInstallationDirectory
  = do args <- getArgs
       let results = getOpt Permute options args
       process results
  where
    process ([InstallDir dir], _, []) = return dir
    process (_,_,errs) = fail ("Invalid commandline parameters:\n" ++ concat errs ++ usageInfo "usage: Hint [OPTION...]" options)

