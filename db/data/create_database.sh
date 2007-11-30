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

create table lastbuilt (
"project"  varchar(50) not null,
"scenario" varchar(50) not null,
"date" date,
primary key ("project", "scenario")
);

create table logs (
"client" varchar(50) not null,
"project"  varchar(50) not null,
"scenario" varchar(50) not null,
"action" varchar(50) not null,
"log" longtext,
"status" boolean,
"date" date default current_timestamp,
primary key ("client", "project", "scenario", "action")
);

create trigger after insert on logs
begin
   insert or replace into lastbuilt values (new.project, new.scenario, new.date);
end;

EOF

