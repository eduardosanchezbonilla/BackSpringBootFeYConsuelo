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
    email VARCHAR(300),
    voice_id INTEGER NOT NULL,
    image VARCHAR(4000),
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

DROP TABLE IF EXISTS feyconsuelo.voice;

CREATE TABLE IF NOT EXISTS feyconsuelo.voice
(
    id SERIAL PRIMARY KEY,
    voice_order INTEGER NOT NULL,
    name VARCHAR(100) NOT NULL,
    image VARCHAR(4000),
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

-- load_stock to dealer
ALTER TABLE feyconsuelo.musician ADD CONSTRAINT fk_voice_musician FOREIGN KEY (voice_id) REFERENCES feyconsuelo.voice (id);