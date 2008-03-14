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
"start_date" date default current_timestamp,
"stop_date" date,
"last_duration" integer default -1,
"duration" integer default -1
);

create trigger set_action_last_duration after insert on logs
   begin
      update logs set last_duration =
        (select duration from logs
                         where client = new.client
                           and project = new.project
                           and scenario = new.scenario
                           and action = new.action
                           and rowid != new.rowid
                         order by job_id desc
                         limit 1)
       where rowid = new.rowid;
   end;

create trigger set_action_duration after update on logs
   begin
      update logs set duration =
       (julianday(new.stop_date)*86000 - julianday(new.start_date)*86000)
       where rowid = new.rowid;
   end;

create table notify (
"project" varchar(50) not null,
"email" varchar(255) unique,
"xmpp" varchar(255) unique
);

EOF
