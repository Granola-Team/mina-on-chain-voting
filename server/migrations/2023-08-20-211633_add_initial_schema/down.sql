-- This file should undo anything in `up.sql`

DROP INDEX IF EXISTS mina_proposals_key_idx;
DROP TABLE IF EXISTS mina_proposals;

DROP TYPE IF EXISTS proposal_version;
DROP TYPE IF EXISTS proposal_category;