cd release
call clean
cd ..
javac -O -g:none -d release hint/Hint.java hint/HintUU.java
cd release
call createJar
cd ..