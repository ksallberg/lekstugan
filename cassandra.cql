// cqlsh --cqlversion="3.4.4" < cassandra.cql

CREATE KEYSPACE pivo
  WITH REPLICATION = {
  'class' : 'SimpleStrategy',
  'replication_factor' : 1
};

CREATE TABLE pivo.lager ( id UUID PRIMARY KEY,
                          name text );

INSERT INTO pivo.lager (id, name)
  VALUES (cfd66ccc-d857-4e90-b1e5-df98a3d40cd6, 'pripps');

SELECT id, name FROM pivo.lager;
