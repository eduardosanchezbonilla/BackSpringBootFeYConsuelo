--
-- modificar columnas
--
ALTER TABLE feyconsuelo.performance ADD COLUMN event_public BOOLEAN NOT NULL default true;

ALTER TABLE feyconsuelo.performance ADD COLUMN repertoire_public BOOLEAN NOT NULL default true;

ALTER TABLE feyconsuelo.performance ADD COLUMN crosshead_public BOOLEAN NOT NULL default true;
