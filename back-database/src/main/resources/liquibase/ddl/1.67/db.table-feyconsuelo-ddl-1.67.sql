--
-- modificar columnas
--
ALTER TABLE feyconsuelo.performance ADD COLUMN bus_data BOOLEAN NOT NULL default false;

ALTER TABLE feyconsuelo.performance ADD COLUMN bus_time TIMESTAMP;

ALTER TABLE feyconsuelo.performance ADD COLUMN bus_location Varchar(200);
