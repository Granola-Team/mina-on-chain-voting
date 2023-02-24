-- Your SQL goes here

CREATE TABLE mina_proposals (
  id SERIAL PRIMARY KEY,
  key TEXT NOT NULL UNIQUE,
  global_start_slot BIGINT NOT NULL,
  global_end_slot BIGINT NOT NULL,
  ledger_hash TEXT
);

CREATE INDEX mina_proposals_key_idx ON mina_proposals (key);

INSERT INTO mina_proposals VALUES (1, 'MIP1', 316140, 320791, 'jxQXzUkst2L9Ma9g9YQ3kfpgB5v5Znr1vrYb1mupakc5y7T89H8'); 