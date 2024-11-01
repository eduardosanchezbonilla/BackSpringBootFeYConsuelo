
ALTER TABLE feyconsuelo.user
ALTER COLUMN firebase_token TYPE VARCHAR(500)[]
USING string_to_array(firebase_token, ',')::VARCHAR(500)[];

