#!/bin/sh

cd $(dirname $0)

SAVADUR_DIR=test-dir/server ./bin/savadur-server &
PID_SERVER=$!
SAVADUR_DIR=test-dir/client ./bin/savadur-client -server &
PID_CLIENT=$!

echo "Press any key to stop"

read

echo "Exit"
echo Kill $PID_SERVER $PID_CLIENT
kill $PID_SERVER $PID_CLIENT
