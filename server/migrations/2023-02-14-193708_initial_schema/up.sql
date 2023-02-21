-- Your SQL goes here

CREATE TABLE mina_proposals (
  id SERIAL PRIMARY KEY,
  key TEXT NOT NULL UNIQUE,
  global_start_slot INTEGER NOT NULL,
  global_end_slot INTEGER NOT NULL
);