echo. Copy the icon files in the gui/icon directory to the release directory
echo. otherwise you won't get icons.

cd release
call clean
cd ..
javac -O -g -d release hint/Hint.java
cd release
call createJar
cd ..
