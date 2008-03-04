#!/bin/sh

cd $(dirname $0)

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

sleep 5

wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

sleep 1

#  Reschedule same job, check output for deleted job
wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default&l=5"

wget --no-proxy "http://localhost:8181/run?p=morzhol&s=default&l=5"

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

TL=test-dir/logs

#  Wait for morzhol regtests log

echo 'Press CTRL-C to kill'

trap 'kill $PID_SERVER $PID_CLIENT; exit' 2

while [ ! -f $ML/regtests ]; do
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

echo === Check log directory $TL

check_file $TL 1-init
check_file $TL 1-make
check_file $TL 1-regtests
check_file $TL 1-pull
check_file $TL 1-version
check_file $TL 2-init
check_file $TL 2-make
check_file $TL 2-regtests
check_file $TL 2-update
check_file $TL 2-version

echo ""
echo "Exit, kill processes"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
