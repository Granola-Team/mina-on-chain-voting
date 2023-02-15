-- Your SQL goes here

CREATE TABLE mina_proposals (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key TEXT NOT NULL,
  start_epoch INTEGER NOT NULL,
  start_slot INTEGER NOT NULL,
  end_epoch INTEGER NOT NULL,
  end_slot INTEGER NOT NULL
);
