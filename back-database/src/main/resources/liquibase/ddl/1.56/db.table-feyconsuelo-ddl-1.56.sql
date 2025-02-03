DROP TABLE IF EXISTS feyconsuelo.repertoire_march_main_soloist;

DROP TABLE IF EXISTS feyconsuelo.repertoire_march_secondary_soloist;

DROP TABLE IF EXISTS feyconsuelo.repertoire_march_solo;

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_solo
(
    id SERIAL NOT NULL PRIMARY KEY,
    march_id INTEGER NOT NULL,
    name VARCHAR(100) NOT NULL,
    solo_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.repertoire_march_solo
    ADD CONSTRAINT fk_repertoire_march_solo_repertoire_march FOREIGN KEY (march_id) REFERENCES feyconsuelo.repertoire_march (id) ON DELETE CASCADE;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_main_soloist
(
    id SERIAL NOT NULL PRIMARY KEY,
    solo_id INTEGER NOT NULL,
    musician_id INTEGER NOT NULL,
    musician_name VARCHAR(300) NOT NULL,
    soloist_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.repertoire_march_main_soloist
    ADD CONSTRAINT fk_repertoire_march_main_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_secondary_soloist
(
    id SERIAL NOT NULL PRIMARY KEY,
    solo_id INTEGER NOT NULL,
    musician_id INTEGER NOT NULL,
    musician_name VARCHAR(300) NOT NULL,
    soloist_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
    );

--

ALTER TABLE feyconsuelo.repertoire_march_secondary_soloist
    ADD CONSTRAINT fk_repertoire_march_secondary_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;
