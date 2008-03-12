#!/bin/sh

cd $(dirname $0)

#  Create newproj repository

cd test-dir/newproj
git init

cat > main.adb <<EOF
procedure Main is
begin
  null;
end Main;
EOF

cat > makefile <<EOF
build:
	gnatmake main

regtests:
	./main
EOF

cat > readme.txt <<EOF
This is a simple test project.
EOF

git add main.adb makefile readme.txt
git ci -m "Initial revision"

cd ../..

#  Write server client.xml configuration file as this is not yet supported
#  by the application.
SCONF=$PWD/test-dir/server/client.xml

echo '<client>' > $SCONF
echo '   <name id="theserver"/>' >> $SCONF
echo '   <endpoint url="http://localhost:8181"/>' >> $SCONF
echo '</client>' >> $SCONF
#######

SAVADUR_DIR=$PWD/test-dir/server ./bin/savadur --server -VV & PID_SERVER=$!
sleep 1
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur --client -VV  \
  --config --id me
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur --client -VV  \
  --config --endpoint http://localhost:8282
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur --client -VV  &
PID_CLIENT=$!

trap 'kill $PID_SERVER $PID_CLIENT; exit' 2

sleep 5

wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

sleep 1

#  Reschedule same job, check output for deleted job
wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

wget --no-proxy "http://localhost:8181/run?p=morzhol&s=default&l=5"

wget --no-proxy "http://localhost:8181/run?p=newproj&s=default&l=5"

echo ""
echo "----------------------------------------------"
echo "CHECK STATUS"
echo "----------------------------------------------"

function check_file() {
   local dir=$1
   local file=$2

   if [ -f $dir/$file ]; then
       echo OK: $file;
   else
       echo NOK: $file;
   fi;
}

if [ "$OS" == "Windows_NT" ]; then
   EXEEXT=.exe
fi;

MP=test-dir/client/work/morzhol
MS=$MP/sources
ML=$MP/log

SCP=test-dir/client/work/style_checker
SCS=$SCP/sources
SCL=$SCP/log

NPP=test-dir/client/work/newproj
NPS=$NPP/sources
NPL=$NPP/log

TL=test-dir/logs

#  Wait for last regtests log

echo 'Press CTRL-C to kill'

while [ ! -f $TL/3-machine-@ENDACTION@ ]; do
    sleep 1
done

#  All initial builds are terminated, update newproj main.adb and rerun the
#  test

cd test-dir/newproj

cat > main.adb <<EOF
with Ada.Text_IO;
procedure Main is
begin
  Ada.Text_IO.Put_Line ("Savadur rocks");
end Main;
EOF

cat > readme.txt <<EOF
Better documentation for this tool.
EOF

git ci -a -m "New main, and documentation"

cd ../..

wget --no-proxy "http://localhost:8181/run?p=newproj&s=default&l=5"

#  Wait for last regtests log

while [ ! -f $TL/4-machine-@ENDACTION@ ]; do
    sleep 1
done

echo === Check for $MP

check_file $MS/.build/debug/lib morzhol.ali
check_file $ML 2-init
check_file $ML 2-make
check_file $ML 2-regtests
check_file $ML 2-update
check_file $ML 2-version

versize=$(wc -w $ML/2-version | cut -c1)

if [ "$versize" == "1" ]; then
    echo OK: version has single word;
else
    echo NOK: version size is $versize;
fi;

committer=$(cat $ML/2-committers_1)

if [ "$committer" == "pascal.obry" ]; then
    echo OK: committer is pascal.obry;
else
    echo NOK: committer is wrong, $committer;
fi;

echo === Check for $SCP

check_file $SCS/obj style_checker.ali
check_file $SCS style_checker$EXEEXT
check_file $SCL 1-init
check_file $SCL 1-make
check_file $SCL 1-regtests
check_file $SCL 1-pull
check_file $SCL 1-version

versize=$(wc -w $SCL/1-version | cut -c1)

if [ "$versize" == "1" ]; then
    echo OK: version has single word;
else
    echo NOK: version size is $versize;
fi;

committer=$(cat $SCL/1-committers_1)

if [ "$committer" == "Pascal Obry <pascal@obry.net>" ]; then
    echo OK: committer is $committer;
else
    echo NOK: committer is wrong, $committer;
fi;

echo === Check for $NPP
check_file $NPS main.ali
check_file $NPS main$EXEEXT
check_file $NPL 3-version
check_file $NPL 4-version
check_file $NPL 4-pull.files_updated

filesu=$(cat $NPL/3-pull.files_updated)

if [ "$filesu" == "" ]; then
    echo OK: files_updated is empty
else
    echo NOK: files_updated is wrong, should be empty
fi;

filesi=$(cat $NPL/4-pull.files_ignored)

if [ "$filesi" == "readme.txt" ]; then
    echo OK: files_ignored is $filesi
else
    echo NOK: files_ignored is wrong, $filesi
fi;

filesu=$(cat $NPL/4-pull.files_updated | tr [:cntrl:] " ")

if [ "$filesu" == "main.adb readme.txt" ]; then
    echo OK: files_updated is $filesu
else
    echo NOK: files_updated is wrong, $filesu
fi;

echo === Check log directory $TL

check_file $TL 1-machine-init
check_file $TL 1-machine-make
check_file $TL 1-machine-regtests
check_file $TL 1-machine-pull
check_file $TL 1-machine-version

check_file $TL 2-machine-init
check_file $TL 2-machine-make
check_file $TL 2-machine-regtests
check_file $TL 2-machine-update
check_file $TL 2-machine-version

check_file $TL 3-machine-init
check_file $TL 3-machine-make
check_file $TL 3-machine-regtests
check_file $TL 3-machine-pull
check_file $TL 3-machine-version

check_file $TL 4-machine-make
check_file $TL 4-machine-regtests
check_file $TL 4-machine-pull
check_file $TL 4-machine-version

echo ""
echo "Exit, kill processes"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
