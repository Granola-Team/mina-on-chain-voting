-- Your SQL goes here

CREATE TABLE mina_proposals (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key TEXT NOT NULL UNIQUE,
  global_start_slot INTEGER NOT NULL,
  global_end_slot INTEGER NOT NULL
);