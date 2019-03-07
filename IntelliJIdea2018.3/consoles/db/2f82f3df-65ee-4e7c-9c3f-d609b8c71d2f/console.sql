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