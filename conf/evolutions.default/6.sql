# --- !Ups
ALTER TABLE gpx
  ADD COLUMN repertoirephotos TEXT
;

# --- !Downs
ALTER TABLE gpx
  DROP COLUMN IF EXISTS repertoirephotos
;
