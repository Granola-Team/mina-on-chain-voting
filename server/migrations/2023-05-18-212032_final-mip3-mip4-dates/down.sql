-- This file should undo anything in `up.sql`
UPDATE mina_proposals SET start_time = 1682769600000, end_time = 1683935820000 WHERE key = 'MIP3';
UPDATE mina_proposals SET start_time = 1682769600000, end_time = 1683935820000 WHERE key = 'MIP4';