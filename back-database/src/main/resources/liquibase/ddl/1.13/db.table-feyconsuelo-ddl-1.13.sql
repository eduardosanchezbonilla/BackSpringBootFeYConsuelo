DROP TABLE IF EXISTS feyconsuelo.inventary;

CREATE TABLE IF NOT EXISTS feyconsuelo.inventary
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    image VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

---------------------------------------------------

DROP TABLE IF EXISTS feyconsuelo.musician_inventary;

CREATE TABLE IF NOT EXISTS feyconsuelo.musician_inventary
(
    musician_id SERIAL NOT NULL,
    inventary_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (musician_id, inventary_id)
);


ALTER TABLE feyconsuelo.musician_inventary
    ADD CONSTRAINT fk_musician_inventary_musician FOREIGN KEY (musician_id) REFERENCES feyconsuelo.musician (id) ON DELETE CASCADE;

ALTER TABLE feyconsuelo.musician_inventary
    ADD CONSTRAINT fk_musician_inventary_inventary FOREIGN KEY (inventary_id) REFERENCES feyconsuelo.inventary (id) ON DELETE CASCADE;