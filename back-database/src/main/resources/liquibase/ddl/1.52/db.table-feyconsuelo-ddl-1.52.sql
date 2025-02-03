
CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_solo
(
    march_id SERIAL NOT NULL,
    id SERIAL NOT NULL,
    solo_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (march_id, id)
);

--

--ALTER TABLE feyconsuelo.repertoire_march_solo
--    ADD CONSTRAINT fk_repertoire_march_solo_repertoire_march FOREIGN KEY (march_id) REFERENCES feyconsuelo.repertoire_march (id) ON DELETE CASCADE;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_main_soloist
(
    solo_id SERIAL NOT NULL,
    musician_id SERIAL NOT NULL,
    musician_name VARCHAR(300) NOT NULL,
    soloist_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (solo_id, musician_id)
);

--

--ALTER TABLE feyconsuelo.repertoire_march_main_soloist
--    ADD CONSTRAINT fk_repertoire_march_main_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_secondary_soloist
(
    solo_id SERIAL NOT NULL,
    musician_id SERIAL NOT NULL,
    musician_name VARCHAR(300) NOT NULL,
    soloist_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (solo_id, musician_id)
    );

--

--ALTER TABLE feyconsuelo.repertoire_march_secondary_soloist
--    ADD CONSTRAINT fk_repertoire_march_secondary_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;