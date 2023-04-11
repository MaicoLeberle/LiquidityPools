CREATE TABLE user_id (
    key text PRIMARY KEY
);

CREATE TYPE currency AS ENUM (
    'ARS',
    'EUR',
    'GBP',
    'USD'
);

CREATE TABLE account (
    asset_name currency,
    asset_amount integer,
    userID text,
    CHECK (asset_amount > 0),
    FOREIGN KEY (userID) REFERENCES user_id(key)
);

CREATE SEQUENCE pool_id
    START WITH 0
    INCREMENT BY 1
    MINVALUE 0
    NO MAXVALUE
    CACHE 1;

CREATE TABLE pool (
    key integer DEFAULT nextval('pool_id') NOT NULL,
    asset_name_A currency,
    asset_amount_A integer,
    asset_name_B currency,
    asset_amount_B integer,
    CHECK (asset_name_A < asset_name_B),
    CHECK (asset_amount_A >= 0),
    CHECK (asset_amount_B >= 0),
    UNIQUE (asset_name_A, asset_name_B)
);

CREATE TABLE liquidity_token (
    poolID integer DEFAULT nextval('pool_id') NOT NULL,
    amount integer
);

CREATE TABLE liquidity_token_ownership (
    poolID integer DEFAULT nextval('pool_id') NOT NULL,
    userID text,
    liquidity_token_amount integer,
    CHECK (liquidity_token_amount >= 0),
    FOREIGN KEY (userID) REFERENCES user_id(key)
);
