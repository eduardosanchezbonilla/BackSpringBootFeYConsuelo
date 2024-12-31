DROP TABLE IF EXISTS feyconsuelo.repertoire_march_rehearsal;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_rehearsal
(
    march_id SERIAL NOT NULL,
    rehearsal_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (march_id, rehearsal_id)
);

--

DROP TABLE IF EXISTS feyconsuelo.repertoire_march_performance;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.repertoire_march_performance
(
    march_id SERIAL NOT NULL,
    performance_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (march_id, performance_id)
);
