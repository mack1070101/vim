ALTER TABLE vehicle_delivery_location add column   valet           BOOLEAN        NOT NULL;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE vehicle_delivery_location_AUD add column   valet           BOOLEAN;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE Vehicle_airport_delivery_fee
  ADD COLUMN migrated BOOLEAN NOT NULL DEFAULT FALSE;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE vehicle_airport_delivery_fee
  ADD COLUMN migrated BOOLEAN NOT NULL DEFAULT FALSE;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE location
  ADD COLUMN place_id VARCHAR(255) DEFAULT NULL;
;-- -. . -..- - / . -. - .-. -.--
SELECT * FROM location where type != "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT * FROM location where type = "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT count(DISTINCT ) FROM location where type = "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT count(DISTINCT id) FROM location where type = "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT *FROM location
where type = "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT * FROM location where airport_code = "SFO";
;-- -. . -..- - / . -. - .-. -.--
SELECT * FROM location where airport_code = "SFO" OR airport_code = "LAX";
;-- -. . -..- - / . -. - .-. -.--
UPDATE location SET place_id = " ChIJVVVVVYx3j4ARP-3NGldc8qQ" WHERE id = 3448;
;-- -. . -..- - / . -. - .-. -.--
UPDATE location set place_id = "TACO" where id = 3444;
;-- -. . -..- - / . -. - .-. -.--
SELECT *FROM location where type = "AIRPORT";
;-- -. . -..- - / . -. - .-. -.--
SELECT  * FROM location WHERE id = 5624;
;-- -. . -..- - / . -. - .-. -.--
select * from vehicle;