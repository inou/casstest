-- -*- mode: sql -*-
DROP KEYSPACE IF EXISTS casstest;

CREATE KEYSPACE IF NOT EXISTS casstest
WITH replication = {'class':'SimpleStrategy', 'replication_factor':'1'};

CREATE TABLE casstest.users(
        id uuid,
        name text,
        PRIMARY KEY(id));

CREATE TABLE casstest.events(
        id timeuuid,
        description text,
        PRIMARY KEY(id));

CREATE TABLE casstest.events1(
        user                        uuid,
        events                      set<timeuuid>,
        PRIMARY KEY(user));

CREATE TABLE casstest.events2(
        user                        uuid,
        event                       timeuuid,
        PRIMARY KEY(user, event));
