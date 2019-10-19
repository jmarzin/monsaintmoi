# --- !Ups
CREATE TABLE gpx (
  id SERIAL NOT NULL PRIMARY KEY,
  idTrek TEXT,
  titre TEXT NOT NULL,
  sousTitre TEXT NOT NULL,
  description TEXT NOT NULL,
  listeMatos TEXT,
  nomFichier TEXT,
  altitudeMinimum INT,
  altitudeMaximum INT,
  ascensionTotale INT,
  descenteTotale INT,
  heureDebut TEXT NOT NULL,
  heureFin TEXT NOT NULL,
  distanceTotale INT,
  depart TEXT,
  arrivee TEXT,
  coordonneesPix TEXT
);

# --- !Downs
DROP TABLE gpx;