cd release
call clean
cd ..
javac -O -g -d release hint/Hint.java
cd release
call createJar
cd ..