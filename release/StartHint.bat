@echo off
rem Expects Helium installation directory parameter
rem There may be more parameters in case of spaces in the directory name (sigh!)

set param=%1
if not (%2) == () set param=%param% %2
if not (%3) == () set param=%param% %3
if not (%4) == () set param=%param% %4
if not (%5) == () set param=%param% %5

rem Set LVMPATH so that helium compiler can see it
set LVMPATH=.;%param%\lib\simple
rem Set LVMPATH and PATH for Hint
start javaw -DPATH="%param%\bin" -DLVMPATH=".;%param%\lib\simple" -jar "%param%\bin\Hint.jar"
exit
