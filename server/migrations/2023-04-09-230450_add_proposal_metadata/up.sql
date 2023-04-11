ALTER TABLE mina_proposals ADD COLUMN title TEXT;
ALTER TABLE mina_proposals ADD COLUMN description TEXT;
ALTER TABLE mina_proposals ADD COLUMN url TEXT;

UPDATE mina_proposals SET title = 'Remove supercharged rewards', description = 'This MIP removes the short-term incentive of supercharged rewards.', url = 'https://github.com/MinaProtocol/MIPs/blob/main/MIPS/mip-remove-supercharged-rewards.md' WHERE key = 'MIP1';