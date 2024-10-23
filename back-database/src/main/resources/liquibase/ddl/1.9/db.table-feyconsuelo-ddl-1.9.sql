--
-- modificar columnas
--
ALTER TABLE feyconsuelo.musician ADD COLUMN birth_date TIMESTAMP NOT NULL DEFAULT CURRENT_DATE;

ALTER TABLE feyconsuelo.musician ADD COLUMN registration_date TIMESTAMP NOT NULL DEFAULT CURRENT_DATE;


