-- Your SQL goes here

ALTER TABLE mina_proposals RENAME COLUMN global_start_slot TO start_time;
ALTER TABLE mina_proposals RENAME COLUMN global_end_slot TO end_time;

UPDATE mina_proposals SET start_time = 1672848000000, end_time = 1673685000000 WHERE key = 'MIP1';