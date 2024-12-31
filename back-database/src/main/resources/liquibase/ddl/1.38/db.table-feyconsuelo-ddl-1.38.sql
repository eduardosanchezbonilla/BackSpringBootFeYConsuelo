DROP TABLE IF EXISTS feyconsuelo.repertoire_category;

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_category
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    repertoire_category_order INTEGER NOT NULL DEFAULT 1,
    image VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--


