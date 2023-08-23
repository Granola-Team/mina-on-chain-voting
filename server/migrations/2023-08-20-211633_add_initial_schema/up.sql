-- Your SQL goes here

CREATE TYPE proposal_version AS ENUM ('V1', 'V2');
CREATE TYPE proposal_category AS ENUM ('Core', 'Networking', 'Interface', 'ERC', 'Cryptography');

CREATE TABLE mina_proposals (
  id SERIAL PRIMARY KEY,
  key TEXT NOT NULL UNIQUE,
  start_time BIGINT NOT NULL,
  end_time BIGINT NOT NULL,
  ledger_hash TEXT,
  category proposal_category NOT NULL,
  version proposal_version NOT NULL DEFAULT 'V2',
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  url TEXT NOT NULL
);

CREATE INDEX mina_proposals_key_idx ON mina_proposals (key);

INSERT INTO mina_proposals VALUES (1, 'MIP1', 1672848000000, 1673685000000, 'jxQXzUkst2L9Ma9g9YQ3kfpgB5v5Znr1vrYb1mupakc5y7T89H8', 'Core', 'V1', 'Remove supercharged rewards', 'Removing the short-term incentive of supercharged rewards.', 'https://github.com/MinaProtocol/MIPs/blob/main/MIPS/mip-remove-supercharged-rewards.md');
INSERT INTO mina_proposals VALUES (3, 'MIP3', 1684562400000, 1685253600000, 'jw8dXuUqXVgd6NvmpryGmFLnRv1176oozHAro8gMFwj8yuvhBeS', 'Cryptography', 'V2', 'Kimchi, a new proof system', 'Kimchi is an update to the proof system currently used by Mina.', 'https://github.com/MinaProtocol/MIPs/blob/main/MIPS/mip-kimchi.md'); 
INSERT INTO mina_proposals VALUES (4, 'MIP4', 1684562400000, 1685253600000, 'jw8dXuUqXVgd6NvmpryGmFLnRv1176oozHAro8gMFwj8yuvhBeS', 'Core', 'V2', 'Easier zkApp programmability on mainnet', 'Adding programmable smart contracts (zkApps) to the Mina protocol.', 'https://github.com/MinaProtocol/MIPs/blob/main/MIPS/mip-zkapps.md')
