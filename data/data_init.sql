INSTALL postgres;
LOAD postgres;

ATTACH 'dbname=shiny_db user=visualisation password=visualisation host=localhost' AS db (TYPE POSTGRES);

CREATE TABLE db.equipment_access(X INTEGER, Y INTEGER, typeeq_id TEXT, distance FLOAT, deuclidienne FLOAT, duree FLOAT, depcom TEXT, pop INTEGER);

INSERT INTO db.equipment_access 
SELECT X,Y,typeeq_id, distance, deuclidienne, duree, depcom, pop FROM 'donnees-2024-reg94.parquet';

-- RESET DATABASE
-- DELETE FROM db.public.equipment_access WHERE 1=1;
-- DROP TABLE db.public.equipment_access;