# --- !Ups
CREATE TABLE materiel (
id SERIAL NOT NULL PRIMARY KEY,
nom TEXT NOT NULL,
description TEXT NOT NULL,
photo TEXT NOT NULL,
poids INT NOT NULL
);

# --- !Downs
DROP TABLE materiel;