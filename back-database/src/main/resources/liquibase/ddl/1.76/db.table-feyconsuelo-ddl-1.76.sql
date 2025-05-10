ALTER TABLE feyconsuelo.user_point_survey_option
    ADD CONSTRAINT fk_user_point_survey_option_survey FOREIGN KEY (survey_id) REFERENCES feyconsuelo.survey (id) ON DELETE CASCADE;


ALTER TABLE feyconsuelo.user_point_survey_option
    ADD CONSTRAINT fk_user_point_survey_option_survey_option FOREIGN KEY (option_id) REFERENCES feyconsuelo.survey_option (id) ON DELETE CASCADE;