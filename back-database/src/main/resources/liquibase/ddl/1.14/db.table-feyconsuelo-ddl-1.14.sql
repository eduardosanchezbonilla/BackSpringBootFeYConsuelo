DROP TABLE IF EXISTS feyconsuelo.musician_inventary;

DROP TABLE IF EXISTS feyconsuelo.inventary;

---------------------------------------------------

DROP TABLE IF EXISTS feyconsuelo.inventory;

CREATE TABLE IF NOT EXISTS feyconsuelo.inventory
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(300) NOT NULL,
    image VARCHAR,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL
);

---------------------------------------------------

DROP TABLE IF EXISTS feyconsuelo.musician_inventory;

CREATE TABLE IF NOT EXISTS feyconsuelo.musician_inventory
(
    musician_id SERIAL NOT NULL,
    inventory_id SERIAL NOT NULL,
    update_user VARCHAR(150) NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    update_date TIMESTAMP NOT NULL,
    delete_date TIMESTAMP NULL,
    PRIMARY KEY (musician_id, inventory_id)
);


ALTER TABLE feyconsuelo.musician_inventory
    ADD CONSTRAINT fk_musician_inventory_musician FOREIGN KEY (musician_id) REFERENCES feyconsuelo.musician (id) ON DELETE CASCADE;

ALTER TABLE feyconsuelo.musician_inventory
    ADD CONSTRAINT fk_musician_inventory_inventory FOREIGN KEY (inventory_id) REFERENCES feyconsuelo.inventory (id) ON DELETE CASCADE;