@echo off
rem Expects Helium installation directory as first parameter
rem Set LVMPATH so that helium compiler can see it
set LVMPATH=.;%~1\lib
rem Set LVMPATH and PATH for Hint
start javaw -DPATH="%~1\bin" -DLVMPATH=".;%~1\lib" -jar "%~1\bin\Hint.jar"
exit

