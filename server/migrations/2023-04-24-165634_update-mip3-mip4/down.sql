-- This file should undo anything in `up.sql`

-- Your SQL goes here
UPDATE mina_proposals SET end_time = 1683932220000 WHERE key = 'MIP3';
UPDATE mina_proposals SET title = 'Add zkApps to protocol', end_time = 1683932220000 WHERE key = 'MIP4';
