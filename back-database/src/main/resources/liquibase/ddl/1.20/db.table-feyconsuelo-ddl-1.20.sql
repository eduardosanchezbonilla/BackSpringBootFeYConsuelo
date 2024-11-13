DROP TABLE IF EXISTS feyconsuelo.video_category;

CREATE TABLE IF NOT EXISTS feyconsuelo.video_category
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

DROP TABLE IF EXISTS feyconsuelo.video;

CREATE TABLE IF NOT EXISTS feyconsuelo.video
(
    id SERIAL PRIMARY KEY,
    video_category_id SERIAL NOT NULL,
    description  VARCHAR(1000) NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

--

ALTER TABLE feyconsuelo.video
    ADD CONSTRAINT fk_video_video_category FOREIGN KEY (video_category_id) REFERENCES feyconsuelo.video_category (id) ON DELETE CASCADE;

