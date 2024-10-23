--
-- modificar columnas
--
ALTER TABLE feyconsuelo.user ADD COLUMN dni VARCHAR(10);
ALTER TABLE feyconsuelo.user ADD COLUMN name VARCHAR(50);
ALTER TABLE feyconsuelo.user ADD COLUMN surname VARCHAR(100);
ALTER TABLE feyconsuelo.user ADD COLUMN direction VARCHAR(500);
ALTER TABLE feyconsuelo.user ADD COLUMN municipality VARCHAR(100);
ALTER TABLE feyconsuelo.user ADD COLUMN province VARCHAR(100);
ALTER TABLE feyconsuelo.user ADD COLUMN image VARCHAR;
ALTER TABLE feyconsuelo.user ADD COLUMN description VARCHAR(4000) ;
