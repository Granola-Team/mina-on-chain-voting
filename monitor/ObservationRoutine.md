## Mina Governance - Observation Routine

### Check the Dashboard
- Go to https://bit.ly/MIP-1_Dashboard & make sure the votes are displayed correctly.
- The App should not display "Something went wrong."
- Check the web-console for errors & the network tab for failed requests.

### Checking the Archive Node
- Log into the Archive Node DB with your tool of choice. (Postico, PGAdmin etc.)
- Go to the "mainnet" database.
- On the "blocks" table get the most recent block by height.
- Go to Mina Explorer (https://minaexplorer.com/) & compare the most recent block with Mina Explorer.
- If they match up - we're still synced.

- From your tool of choice run the following queries:

```
SELECT height, state_hash FROM blocks
WHERE height < (SELECT MAX(height) FROM blocks) - 290 AND chain_status = 'pending';
```
- This query should return no entry.

```
SELECT count( * )
FROM (SELECT h::int FROM generate_series(1 , (select max(height) from blocks)) h
LEFT JOIN blocks b
ON h = b.height where b.height is null) as v;
```

- This query should return 0.

```
Select count(*) from blocks where parent_id is null
```

- This query should return 1.

##### If there are any missing blocks, use the following tools and commands in your archive node instance to check and add blocks:
- mina-missing-blocks-auditor -- returns state hashes of missing blocks from the archive database.
- mina-extract-blocks -- extracts all blocks from said database or a chain if the command is provided with `--start-state-hash` and or `--end-state-hash`.
- mina-archive-blocks -- writes blocks to the database.
- mina-replayer -- replays transactions from the archive node.

#### If these steps don't return any unexpected values - we can assume we're still live & synced.
