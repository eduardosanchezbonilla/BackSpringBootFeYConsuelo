DROP TABLE IF EXISTS feyconsuelo.musician_rehearsal;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.musician_rehearsal
(
    musician_id SERIAL NOT NULL,
    rehearsal_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (musician_id, rehearsal_id)
);

--

DROP TABLE IF EXISTS feyconsuelo.musician_performance;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.musician_performance
(
    musician_id SERIAL NOT NULL,
    performance_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (musician_id, performance_id)
);
