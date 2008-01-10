#!/bin/sh

DATABASE_NAME="logs.db"

SCHEMA_DB="schema-sqlite.sql"
INITIAL_DATA="initial-data.sql"

SQLITE=sqlite3

rm -f ${DATABASE_NAME}

cat <<EOF | ${SQLITE} ${DATABASE_NAME}
create table sessions (
"client" varchar(50) not null,
login_date date default current_timestamp,
logout_date date default current_timestamp
);

create table job_id (
id integer primary key autoincrement
);

create table lastbuilt (
"client" varchar(50) not null,
"project"  varchar(50) not null,
"scenario" varchar(50) not null,
"status" boolean,
"job_id" integer,
"date" date default current_timestamp
);

create table logs (
"client" varchar(50) not null,
"project"  varchar(50) not null,
"scenario" varchar(50) not null,
"action" varchar(50) not null,
"log" longtext,
"status" boolean,
"job_id" integer,
"date" date default current_timestamp
);

create table notify (
"project" varchar(50) not null,
"email" varchar(255) unique,
"xmpp" varchar(255) unique
);

EOF
