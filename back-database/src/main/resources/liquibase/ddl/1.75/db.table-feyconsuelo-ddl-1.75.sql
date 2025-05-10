DROP TABLE IF EXISTS feyconsuelo.user_point_survey_option;

CREATE TABLE IF NOT EXISTS feyconsuelo.user_point_survey_option
(
    username VARCHAR(50) NOT NULL,
    survey_id SERIAL NOT NULL,
    option_id SERIAL NOT NULL,
    points INTEGER NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (username, survey_id, option_id)
);

--
