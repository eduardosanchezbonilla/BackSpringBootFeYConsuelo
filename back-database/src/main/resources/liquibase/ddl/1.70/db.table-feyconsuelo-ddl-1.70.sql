DROP TABLE IF EXISTS feyconsuelo.contract_group;

CREATE TABLE IF NOT EXISTS feyconsuelo.contract_group
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    google_id VARCHAR(300) NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);
