
CREATE TABLE IF NOT EXISTS feyconsuelo.user
(
    username VARCHAR(50) NOT NULL  PRIMARY KEY,
    password VARCHAR(200) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

CREATE TABLE IF NOT EXISTS feyconsuelo.user_roles
(
    username VARCHAR(50) NOT NULL,
    role VARCHAR(50) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (username, role)
);

ALTER TABLE feyconsuelo.user_roles
    ADD CONSTRAINT fk_user_roles_user FOREIGN KEY (username) REFERENCES feyconsuelo.user (username) ON DELETE CASCADE;