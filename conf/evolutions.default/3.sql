# --- !Ups
ALTER TABLE gpx
ADD COLUMN typegpx CHAR (1),
ALTER idTrek TYPE BIGINT USING 0
;

# --- !Downs
ALTER TABLE gpx
  DROP COLUMN IF EXISTS typegpx,
  ALTER idTrek TYPE TEXT
;