#!/bin/sh

cd $(dirname $0)

#  Write server client.xml configuration file as this is not yet supported
#  by the application.
SCONF=$PWD/test-dir/server/client.xml

echo '<client>' > $SCONF
echo '<name id="theserver"/>' >> $SCONF
echo '<endpoint url="http://localhost:8181"/>' >> $SCONF
echo '</client>' >> $SCONF
#######

SAVADUR_DIR=$PWD/test-dir/server ./bin/savadur-server -VV & PID_SERVER=$!
sleep 1
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur-client -VV --server \
  --config --id me
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur-client -VV --server \
  --config --endpoint http://localhost:8282
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur-client -VV --server &
PID_CLIENT=$!

sleep 5

wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

sleep 1

#  Reschedule same job, check output for deleted job
wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

wget --no-proxy "http://localhost:8181/run?p=morzhol&s=default&l=5"

echo "Press any key to stop (when no more activity)"
read

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

echo === Check for $MP

check_file $MS/.build/debug/lib morzhol.ali
check_file $ML init
check_file $ML make
check_file $ML regtests
check_file $ML update
check_file $ML version

versize=$(wc -w $ML/version | cut -c1)

if [ "$versize" == "1" ]; then
    echo OK: version has single word;
else
    echo NOK: version size is $versize;
fi;

committer=$(cat $ML/committers_1)

if [ "$committer" == "pascal.obry" ]; then
    echo OK: committer is pascal.obry;
else
    echo NOK: committer is wrong, $committer;
fi;

echo === Check for $SCP

check_file $SCS/obj style_checker.ali
check_file $SCS style_checker$EXEEXT
check_file $SCL init
check_file $SCL make
check_file $SCL regtests
check_file $SCL pull
check_file $SCL version

versize=$(wc -w $SCL/version | cut -c1)

if [ "$versize" == "1" ]; then
    echo OK: version has single word;
else
    echo NOK: version size is $versize;
fi;

committer=$(cat $SCL/committers_1)

if [ "$committer" == "Pascal Obry <pascal@obry.net>" ]; then
    echo OK: committer is $committer;
else
    echo NOK: committer is wrong, $committer;
fi;

echo ""
echo "Exit, kill processes"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
