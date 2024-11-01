
DROP TABLE IF EXISTS feyconsuelo.user_partiture_request;

CREATE TABLE IF NOT EXISTS feyconsuelo.user_partiture_request
(
    id SERIAL PRIMARY KEY,
    username VARCHAR(50)  NOT NULL,
    partiture_request VARCHAR(500) NOT NULL,
    readed BOOLEAN,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);


ALTER TABLE feyconsuelo.user_partiture_request
    ADD CONSTRAINT fk_user_partiture_request_user FOREIGN KEY (username) REFERENCES feyconsuelo.user (username) ON DELETE CASCADE;

