# --- !Ups
CREATE TABLE trekmateriel (
  id SERIAL NOT NULL PRIMARY KEY,
  idtrek BIGINT,
  idmateriel BIGINT
);

# --- !Downs
DROP TABLE trekmateriel;