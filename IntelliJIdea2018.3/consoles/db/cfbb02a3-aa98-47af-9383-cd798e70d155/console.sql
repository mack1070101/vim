CREATE TABLE vehicle_delivery_location_AUD
(
  id                               INT AUTO_INCREMENT,
  REV                              INT            NOT NULL,
  vehicle_id                       INT            NOT NULL,
  place_id                         VARCHAR(255)   NOT NULL,
  key_exchange_method              VARCHAR(255)   NOT NULL,
  key_exchange_method_instructions VARCHAR(255)   NULL,
  delivery_location_type           VARCHAR(255)   NOT NULL,
  fee_currency                     CHAR(3)        NOT NULL,
  fee_amount                       DECIMAL(10, 2) NOT NULL,
  enabled                          BOOLEAN        NOT NULL,
  instructions                     TEXT           NULL,
  created                          DATETIME       NOT NULL,
  modified                         DATETIME       NOT NULL,

  REVTYPE                          INT(4)         NOT NULL,

  PRIMARY KEY (id, REV)
)
  ENGINE = InnoDB
  DEFAULT CHARSET = utf8
  COLLATE = utf8_unicode_ci;

CREATE TABLE vehicle_delivery_location
(
  id                                  INT AUTO_INCREMENT PRIMARY KEY,
  vehicle_id                          INT            NOT NULL,
  place_id                            VARCHAR(255)   NOT NULL,
  key_exchange_method                 VARCHAR(255)   NOT NULL,
  key_exchange_method_instructions    VARCHAR(255)   NULL,
  delivery_location_type              VARCHAR(255)   NOT NULL,
  fee_currency                        CHAR(3)        NOT NULL,
  fee_amount                          DECIMAL(10, 2) NOT NULL,
  instructions                        TEXT           NULL,
  enabled                             BOOLEAN        NOT NULL,
  created                             DATETIME       NOT NULL,
  modified                            DATETIME       NOT NULL,

  unique index (vehicle_id, place_Id)
)
  ENGINE = InnoDB
  DEFAULT CHARSET = utf8
  COLLATE = utf8_unicode_ci;
