--
-- modificar columnas
--
ALTER TABLE feyconsuelo.video_category ADD COLUMN date TIMESTAMP NOT NULL DEFAULT CURRENT_DATE;

ALTER TABLE feyconsuelo.video_category ADD COLUMN image_thumbnail VARCHAR;
