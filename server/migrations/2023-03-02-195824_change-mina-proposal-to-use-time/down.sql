-- This file should undo anything in `up.sql`
ALTER TABLE mina_proposals RENAME COLUMN start_time TO global_start_slot;
ALTER TABLE mina_proposals RENAME COLUMN end_time TO global_end_slot;

UPDATE mina_proposals SET global_start_slot = 316160, global_end_slot = 320804 WHERE key = 'MIP1';