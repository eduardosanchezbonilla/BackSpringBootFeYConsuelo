
DROP TABLE IF EXISTS feyconsuelo.contact_request;

CREATE TABLE IF NOT EXISTS feyconsuelo.contact_request
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(500)  NOT NULL,
    phoneNumber VARCHAR(50),
    email VARCHAR(2000),
    message VARCHAR NOT NULL,
    readed BOOLEAN,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);
