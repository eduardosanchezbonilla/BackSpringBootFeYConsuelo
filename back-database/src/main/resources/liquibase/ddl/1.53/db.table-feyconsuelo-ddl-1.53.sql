
ALTER TABLE feyconsuelo.repertoire_march_solo
    ADD CONSTRAINT fk_repertoire_march_solo_repertoire_march FOREIGN KEY (march_id) REFERENCES feyconsuelo.repertoire_march (id) ON DELETE CASCADE;

--

--ALTER TABLE feyconsuelo.repertoire_march_main_soloist
--    ADD CONSTRAINT fk_repertoire_march_main_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;

--

--ALTER TABLE feyconsuelo.repertoire_march_secondary_soloist
--    ADD CONSTRAINT fk_repertoire_march_secondary_soloist_repertoire_march_solo FOREIGN KEY (solo_id) REFERENCES feyconsuelo.repertoire_march_solo (id) ON DELETE CASCADE;