DROP TABLE IF EXISTS feyconsuelo.repertoire_march_type;

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_type
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    repertoire_march_type_order INTEGER NOT NULL DEFAULT 1,
    image VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--


