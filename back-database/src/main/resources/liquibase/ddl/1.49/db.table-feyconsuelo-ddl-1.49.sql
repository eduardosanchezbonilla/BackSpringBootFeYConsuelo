--
-- modificar columnas
--
ALTER TABLE feyconsuelo.musician ADD COLUMN phoneNumber VARCHAR(10);

ALTER TABLE feyconsuelo.musician ADD COLUMN image_thumbnail VARCHAR;

ALTER TABLE feyconsuelo.user ADD COLUMN phoneNumber VARCHAR(10);

ALTER TABLE feyconsuelo.user ADD COLUMN image_thumbnail VARCHAR;
