DROP TABLE IF EXISTS feyconsuelo.repertoire_march;

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march
(
    id SERIAL PRIMARY KEY,
    category_id SERIAL NOT NULL,
    type_id SERIAL NOT NULL,
    name VARCHAR(300) NOT NULL,
    author VARCHAR(300) NOT NULL,
    description VARCHAR,
    image VARCHAR,
    yputube_id VARCHAR(100) NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.repertoire_march
    ADD CONSTRAINT fk_repertoire_march_repertoire_category FOREIGN KEY (category_id) REFERENCES feyconsuelo.repertoire_category (id);

ALTER TABLE feyconsuelo.repertoire_march
    ADD CONSTRAINT fk_repertoire_march_repertoire_march_type FOREIGN KEY (type_id) REFERENCES feyconsuelo.repertoire_march_type (id);


