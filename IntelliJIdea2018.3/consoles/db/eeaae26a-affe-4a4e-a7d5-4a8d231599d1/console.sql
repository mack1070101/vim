/* pre deploy changes */
ALTER TABLE vehicle_airport_delivery_fee
  ADD COLUMN migrated BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE location
  ADD COLUMN place_id VARCHAR(255) DEFAULT NULL;

SELECT * FROM location where airport_code = "SFO" OR airport_code = "LAX";

SELECT *FROM location where type = "AIRPORT";

UPDATE location SET place_id = " ChIJVVVVVYx3j4ARP-3NGldc8qQ" WHERE id = 3448);
UPDATE location set place_id = "TACO" where id = 3444;

SELECT  * FROM location WHERE id = 5624 ;