--
-- modificar columnas
--
ALTER TABLE feyconsuelo.musician ADD COLUMN unregistration_date TIMESTAMP ;

ALTER TABLE feyconsuelo.musician ADD COLUMN date_last_notification_non_assists_streak_rehearsals TIMESTAMP NOT NULL DEFAULT CURRENT_DATE;


