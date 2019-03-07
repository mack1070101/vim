select * from driver;
;-- -. . -..- - / . -. - .-. -.--
SELECT * FROM dunlop_test.edmunds_vehicle_definition;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE claim_details
  ADD COLUMN vehicle_fluids_leaking VARCHAR(255),
  ADD COLUMN vehicle_airbags_deployed VARCHAR(255),
  ADD COLUMN vehicle_location VARCHAR(255),
  ADD COLUMN vehicle_location_description text,
  ADD COLUMN injuries_involved VARCHAR(255),
  ADD COLUMN third_party_vehicle_license_plate VARCHAR(255),
  ADD COLUMN third_party_vehicle_license_plate_state VARCHAR(255),
  ADD COLUMN third_party_vehicle_make VARCHAR(255),
  ADD COLUMN third_party_vehicle_model VARCHAR(255),
  ADD COLUMN third_party_vehicle_year INT(11),
  ADD COLUMN third_party_license_number VARCHAR(255),
  ALGORITHM = INPLACE,
  LOCK = NONE;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE damage_report
  ADD COLUMN damage_cause_theft_or_vandalism VARCHAR(255),
  ADD COLUMN damage_cause_natural_disaster VARCHAR(255),
  ADD COLUMN damage_type_selection VARCHAR(255),
  ADD COLUMN damage_subtype_selection VARCHAR(255),
  ALGORITHM = INPLACE,
  LOCK = NONE;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE damage_report_owner_details
  ADD COLUMN appoint_turo_as_power_of_attorney BOOLEAN,
  ADD COLUMN text_messaging_approved BOOLEAN,
  ADD COLUMN additional_information text,
  ADD COLUMN overall_claim_response VARCHAR(255),
  ADD COLUMN overall_claim_description text,
  ALGORITHM = INPLACE,
  LOCK = NONE;
;-- -. . -..- - / . -. - .-. -.--
ALTER TABLE damage_report_renter_details
  ADD COLUMN appoint_turo_as_power_of_attorney BOOLEAN,
  ADD COLUMN text_messaging_approved BOOLEAN,
  ADD COLUMN additional_information text,
  ADD COLUMN overall_claim_response VARCHAR(255),
  ADD COLUMN overall_claim_description text,
  ALGORITHM = INPLACE,
  LOCK = NONE;
;-- -. . -..- - / . -. - .-. -.--
CREATE TABLE vehicle_delivery_location
(
  id                     INT AUTO_INCREMENT PRIMARY KEY,
  vehicle_id             INT            NOT NULL,
  place_id               VARCHAR(255)   NOT NULL,
  key_exchange_method    VARCHAR(255)   NOT NULL,
  delivery_location_type VARCHAR(255)   NOT NULL,
  fee_currency           CHAR(3)        NOT NULL,
  fee_amount             DECIMAL(10, 2) NOT NULL,
  enabled                BOOLEAN        NOT NULL,
  created                DATETIME       NOT NULL,
  modified               DATETIME       NOT NULL
)
  ENGINE = InnoDB
  DEFAULT CHARSET = utf8
  COLLATE = utf8_unicode_ci;