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

SAVADUR_DIR=$PWD/test-dir/
export SAVADUR_DIR

echo ./bin/savadur --server -VV & PID_SERVER=$!
./bin/savadur --server -VV & PID_SERVER=$!
sleep 1
./bin/savadur --client -VV --config --id me
./bin/savadur --client -VV --config --endpoint http://localhost:8282
echo ./bin/savadur --client -VV  & PID_CLIENT=$!
./bin/savadur --client -VV  & PID_CLIENT=$!

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
   local exists=$3

   if [ "$exists" = "true" ]; then
       op='';
   else
       op='!';
   fi;

   if [ $op -f $dir/$file ]; then
       echo -n OK: $file
       if [ "$exists" = "false" ]; then
	   echo ", does not exist"
       else
	   echo
       fi;
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

while [ ! -f $TL/3-machine-@endaction@ ]; do
    sleep 1
done

#  All initial builds are terminated, update newproj main.adb and
#  readme.txt and rerun the test

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

while [ ! -f $TL/4-machine-@endaction@ ]; do
    sleep 1
done

#  Now update only the readme.txt (which is ignored), check that we do not
#  run the make/regtests

cd test-dir/newproj

cat > readme.txt <<EOF
Better documentation for this tool.
Should not trigger the build.
EOF

git ci -a -m "Minor documentation update"

cd ../..

wget --no-proxy "http://localhost:8181/run?p=newproj&s=default&l=5"

#  Wait for last regtests log

while [ ! -f $TL/5-machine-@endaction@ ]; do
    sleep 1
done

echo === Check for $MP

check_file $MS/.build/debug/lib morzhol.ali true
check_file $ML 2-init true
check_file $ML 2-make true
check_file $ML 2-regtests true
check_file $ML 2-update false
check_file $ML 2-version true

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

check_file $SCS/obj style_checker.ali true
check_file $SCS style_checker$EXEEXT true
check_file $SCL 1-init true
check_file $SCL 1-make true
check_file $SCL 1-regtests true
check_file $SCL 1-pull false
check_file $SCL 1-version true

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
check_file $NPS main.ali true
check_file $NPS main$EXEEXT true
check_file $NPL 3-version true
check_file $NPL 4-version true
check_file $NPL 4-pull.files_updated true
check_file $NPL 5-pull true
check_file $NPL 5-pull.files_updated true
check_file $NPL 5-make false
check_file $NPL 5-regtests false

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

check_file $TL 1-machine-init true
check_file $TL 1-machine-make true
check_file $TL 1-machine-regtests true
check_file $TL 1-machine-pull false
check_file $TL 1-machine-version true

check_file $TL 2-machine-init true
check_file $TL 2-machine-make true
check_file $TL 2-machine-regtests true
check_file $TL 2-machine-update false
check_file $TL 2-machine-version true

check_file $TL 3-machine-init true
check_file $TL 3-machine-make true
check_file $TL 3-machine-regtests true
check_file $TL 3-machine-pull false
check_file $TL 3-machine-version true

check_file $TL 4-machine-make true
check_file $TL 4-machine-regtests true
check_file $TL 4-machine-pull true
check_file $TL 4-machine-version true

echo ""
echo "Exit, kill processes"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
