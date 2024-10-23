DROP TABLE IF EXISTS feyconsuelo.user_partiture_group;

CREATE TABLE IF NOT EXISTS feyconsuelo.user_partiture_group
(
    username VARCHAR(50) NOT NULL,
    partiture_group_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (username, partiture_group_id)
);
