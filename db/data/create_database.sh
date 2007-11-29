#!/bin/sh

DATABASE_NAME="logs.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-data.sql"

SQLITE=sqlite3

cat <<EOF | ${SQLITE} ${DATABASE_NAME}
create table logs (
"client" varchar(50) not null primary key,
"scenario" varchar(50) not null,
"action" varchar(50) not null,
"log" longtext,
"date" date default current_timestamp
);
EOF

