--
-- modificar columnas
--
ALTER TABLE feyconsuelo.repertoire_march_rehearsal ADD COLUMN march_numbers INTEGER NOT NULL DEFAULT 1;

ALTER TABLE feyconsuelo.repertoire_march_performance ADD COLUMN march_numbers INTEGER NOT NULL DEFAULT 1;
