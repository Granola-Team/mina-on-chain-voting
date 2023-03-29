CREATE TYPE proposal_version AS ENUM ('V1', 'V2');

ALTER TABLE mina_proposals ADD COLUMN version proposal_version NOT NULL DEFAULT 'V2';

UPDATE mina_proposals SET version = 'V1' WHERE key = 'MIP1';