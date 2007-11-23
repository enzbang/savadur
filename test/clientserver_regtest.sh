#!/bin/sh

cd $(dirname $0)

SAVADUR_DIR=$PWD/test-dir/server ./bin/savadur-server &
PID_SERVER=$!
SAVADUR_DIR=$PWD/test-dir/client ./bin/savadur-client -server &
PID_CLIENT=$!

sleep 5

wget --no-proxy "http://localhost:8181/run?p=style_checker&s=default"

echo "Press any key to stop"

read

echo "Exit"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
