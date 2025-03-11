DROP TABLE IF EXISTS feyconsuelo.crosshead_march_performance;

DROP TABLE IF EXISTS feyconsuelo.crosshead_performance;

CREATE TABLE IF NOT EXISTS feyconsuelo.crosshead_performance
(
    id SERIAL NOT NULL PRIMARY KEY,
    performance_id INTEGER NOT NULL,
    street VARCHAR(100) NOT NULL,
    street_order INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.crosshead_performance
    ADD CONSTRAINT fk_crosshead_performance_performance FOREIGN KEY (performance_id) REFERENCES feyconsuelo.performance (id) ON DELETE CASCADE;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.crosshead_march_performance
(
    id SERIAL NOT NULL PRIMARY KEY,
    crosshead_id INTEGER NOT NULL,
    march_id INTEGER NOT NULL,
    march_name VARCHAR(300) NOT NULL,
    march_order INTEGER NOT NULL,
    annotations VARCHAR(500),
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.crosshead_march_performance
    ADD CONSTRAINT fk_crosshead_march_performance_crosshead_performance FOREIGN KEY (crosshead_id) REFERENCES feyconsuelo.crosshead_performance (id) ON DELETE CASCADE;

--
