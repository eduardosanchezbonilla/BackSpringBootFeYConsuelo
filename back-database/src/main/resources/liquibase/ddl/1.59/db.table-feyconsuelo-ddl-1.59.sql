
DROP TABLE IF EXISTS feyconsuelo.suggestion_box;

CREATE TABLE IF NOT EXISTS feyconsuelo.suggestion_box
(
    id SERIAL PRIMARY KEY,
    username VARCHAR(50)  NOT NULL,
    suggestion VARCHAR(4000) NOT NULL,
    readed BOOLEAN,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);


ALTER TABLE feyconsuelo.suggestion_box
    ADD CONSTRAINT fk_suggestion_box_user FOREIGN KEY (username) REFERENCES feyconsuelo.user (username) ON DELETE CASCADE;

