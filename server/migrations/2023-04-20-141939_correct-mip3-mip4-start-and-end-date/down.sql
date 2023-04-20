-- This file should undo anything in `up.sql`
UPDATE mina_proposals SET start_time = 2000000000000, end_time = 2000000000000 WHERE key = 'MIP3';
UPDATE mina_proposals SET start_time = 2000000000000, end_time = 2000000000000 WHERE key = 'MIP4';
