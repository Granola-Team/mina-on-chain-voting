UPDATE mina_proposals SET title = NULL, description = NULL, url = NULL WHERE key = 'MIP1';

ALTER TABLE mina_proposals DROP COLUMN title;
ALTER TABLE mina_proposals DROP COLUMN description;
ALTER TABLE mina_proposals DROP COLUMN url;