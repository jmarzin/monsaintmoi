# --- !Ups
ALTER TABLE materiel
  ADD COLUMN reforme BOOLEAN
;

# --- !Downs
ALTER TABLE materiel
  DROP COLUMN IF EXISTS reforme
;