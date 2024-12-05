DROP TABLE IF EXISTS feyconsuelo.performance;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.performance
(
    id SERIAL PRIMARY KEY,
    date DATE NOT NULL,
    start_time TIMESTAMP NOT NULL,
    end_time TIMESTAMP NOT NULL,
    title VARCHAR(100) NOT NULL,
    description VARCHAR(1000),
    performance_type VARCHAR(100) NOT NULL,
    voice_id_list INTEGER[] NOT NULL,
    location VARCHAR(200),
    municipality VARCHAR(100),
    province VARCHAR(100),
    image VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

CREATE INDEX in_date_performance ON feyconsuelo.performance USING btree (date);
