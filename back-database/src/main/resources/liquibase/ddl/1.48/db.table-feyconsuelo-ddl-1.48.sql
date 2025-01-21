--
-- modificar columnas
--
ALTER TABLE feyconsuelo.repertoire_march_rehearsal ADD COLUMN march_order INTEGER NOT NULL DEFAULT 0;

ALTER TABLE feyconsuelo.repertoire_march_performance ADD COLUMN march_order INTEGER NOT NULL DEFAULT 0;
