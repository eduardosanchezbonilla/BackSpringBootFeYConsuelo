DROP TABLE IF EXISTS feyconsuelo.musician;

CREATE TABLE IF NOT EXISTS feyconsuelo.musician
(
    id SERIAL PRIMARY KEY,
    dni VARCHAR(10) NOT NULL,
    name VARCHAR(50) NOT NULL,
    surname VARCHAR(100) NOT NULL,
    direction VARCHAR(500) NOT NULL,
    municipality VARCHAR(100) NOT NULL,
    province VARCHAR(100) NOT NULL,
    voiceRequest VARCHAR(50) NOT NULL,
    image VARCHAR(4000) NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);
CREATE UNIQUE INDEX in_feyconsuelo_unique ON feyconsuelo.musician(dni);
