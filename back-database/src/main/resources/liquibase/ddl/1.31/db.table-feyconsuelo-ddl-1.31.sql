DROP TABLE IF EXISTS feyconsuelo.rehearsal;

--

CREATE TABLE IF NOT EXISTS feyconsuelo.rehearsal
(
    id SERIAL PRIMARY KEY,
    date DATE NOT NULL,
    start_time TIMESTAMP NOT NULL,
    end_time TIMESTAMP NOT NULL,
    description VARCHAR(1000),
    voice_id_list INTEGER[] NOT NULL,
    location VARCHAR(200),
    municipality VARCHAR(100),
    province VARCHAR(100),
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

CREATE INDEX in_date_rehearsal ON feyconsuelo.rehearsal USING btree (date);
