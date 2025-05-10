DROP TABLE IF EXISTS feyconsuelo.survey_option;

DROP TABLE IF EXISTS feyconsuelo.survey;

CREATE TABLE IF NOT EXISTS feyconsuelo.survey
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    type VARCHAR(100) NOT NULL,
    description VARCHAR,
    question VARCHAR(500) NOT NULL,
    is_public BOOLEAN NOT NULL,
    is_open BOOLEAN NOT NULL,
    is_finished BOOLEAN NOT NULL,
    image VARCHAR,
    image_thumbnail VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

CREATE TABLE IF NOT EXISTS feyconsuelo.survey_option
(
    id SERIAL PRIMARY KEY,
    survey_id SERIAL NOT NULL,
    name VARCHAR(300) NOT NULL,
    description VARCHAR,
    youtube_id VARCHAR(100),
    option_order INTEGER NOT NULL DEFAULT 1,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.survey_option
    ADD CONSTRAINT fk_survey_option_survey FOREIGN KEY (survey_id) REFERENCES feyconsuelo.survey (id) ON DELETE CASCADE;

