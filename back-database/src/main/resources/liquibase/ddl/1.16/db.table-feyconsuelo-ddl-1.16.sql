
DROP TABLE IF EXISTS feyconsuelo.musician_partiture_request;

CREATE TABLE IF NOT EXISTS feyconsuelo.musician_partiture_request
(
    id SERIAL PRIMARY KEY,
    musician_id SERIAL NOT NULL,
    partiture_request VARCHAR(500) NOT NULL,
    readed Boolean,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);


ALTER TABLE feyconsuelo.musician_partiture_request
    ADD CONSTRAINT fk_musician_partiture_request_musician FOREIGN KEY (musician_id) REFERENCES feyconsuelo.musician (id) ON DELETE CASCADE;

-----------------------------------------------------------------------------------------------------------------------

ALTER TABLE feyconsuelo.user ADD COLUMN firebase_token VARCHAR(500);