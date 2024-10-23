
ALTER TABLE feyconsuelo.user_partiture_group
    ADD CONSTRAINT fk_user_partiture_group_user FOREIGN KEY (username) REFERENCES feyconsuelo.user (username) ON DELETE CASCADE;

ALTER TABLE feyconsuelo.user_partiture_group
    ADD CONSTRAINT fk_user_partiture_group_partiture_group FOREIGN KEY (partiture_group_id) REFERENCES feyconsuelo.partiture_group (id) ON DELETE CASCADE;