use crate::consts::*;
use crate::graphql::*;
use anyhow::{anyhow, bail, Result};
use chrono::{DateTime, FixedOffset};
use graphql_client::GraphQLQuery;
use rust_decimal::prelude::FromPrimitive;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LedgerAccountJson {
    pub pk: String,
    pub balance: String,
    pub delegate: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Delegator {
    pub pk: String,
    pub balance: String,
    pub percentage: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DelegatorPayment {
    pub expected: Decimal,
    pub performed: Decimal,
    pub diff: Decimal,
}

pub async fn get_staking_data(
    endpoint: &str,
    epoch: i64,
) -> Result<(String, String, String, Vec<LedgerAccount>)> {
    let request_body = StakingData::build_query(staking_data::Variables {});
    let data: staking_data::ResponseData = graphql_query(endpoint, &request_body).await?;

    let best_chain = match &data.best_chain {
        None => bail!("best_chain is None"),
        Some(best_chain) => match best_chain.len() == 1 {
            false => bail!("should only have 1 best_chain"),
            true => &best_chain[0],
        },
    };
    let best_epoch = &best_chain.protocol_state.consensus_state.epoch;

    let (seed, total_currency, ledger_hash) = if best_epoch != &epoch.to_string() {
        let request_body =
            StakingDataExplorer::build_query(staking_data_explorer::Variables { epoch });
        let data: staking_data_explorer::ResponseData =
            graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;

        let explorer_staking_epoch_data = data.blocks[0]
            .as_ref()
            .ok_or(anyhow!("no block"))?
            .protocol_state
            .as_ref()
            .ok_or(anyhow!("no protocol state"))?
            .consensus_state
            .as_ref()
            .ok_or(anyhow!("no consensus state"))?
            .staking_epoch_data
            .as_ref()
            .ok_or(anyhow!("no staking epoch data"))?;

        let ledger = &explorer_staking_epoch_data
            .ledger
            .as_ref()
            .ok_or(anyhow!("no ledger"))?;
        let seed = explorer_staking_epoch_data
            .seed
            .as_ref()
            .ok_or(anyhow!("no seed"))?;
        let total_currency = &ledger
            .total_currency
            .as_ref()
            .ok_or(anyhow!("no total currency"))?;
        let ledger_hash = explorer_staking_epoch_data
            .ledger
            .as_ref()
            .ok_or(anyhow!("no ledger"))?
            .hash
            .as_ref()
            .ok_or(anyhow!("no hash"))?;

        (
            seed.clone(),
            total_currency.to_string(),
            ledger_hash.clone(),
        )
    } else {
        let staking_epoch_data = &best_chain.protocol_state.consensus_state.staking_epoch_data;
        let seed = &staking_epoch_data.seed;
        let total_currency = &staking_epoch_data.ledger.total_currency;
        let ledger_hash = &staking_epoch_data.ledger.hash;

        (seed.clone(), total_currency.clone(), ledger_hash.clone())
    };

    let delegators = get_delegators(&ledger_hash).await?;

    Ok((seed, total_currency, ledger_hash, delegators))
}

pub async fn get_partial_staking_data_from_mina_explorer(
    epoch: i64,
) -> Result<(String, String, String)> {
    let request_body = StakingDataExplorer::build_query(staking_data_explorer::Variables { epoch });
    let data: staking_data_explorer::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;

    let explorer_staking_epoch_data = data.blocks[0]
        .as_ref()
        .ok_or(anyhow!("no block"))?
        .protocol_state
        .as_ref()
        .ok_or(anyhow!("no protocol state"))?
        .consensus_state
        .as_ref()
        .ok_or(anyhow!("no consensus state"))?
        .staking_epoch_data
        .as_ref()
        .ok_or(anyhow!("no staking epoch data"))?;

    let ledger = &explorer_staking_epoch_data
        .ledger
        .as_ref()
        .ok_or(anyhow!("no ledger"))?;
    let seed = explorer_staking_epoch_data
        .seed
        .as_ref()
        .ok_or(anyhow!("no seed"))?;
    let total_currency = &ledger
        .total_currency
        .as_ref()
        .ok_or(anyhow!("no total currency"))?;
    let ledger_hash = explorer_staking_epoch_data
        .ledger
        .as_ref()
        .ok_or(anyhow!("no ledger"))?
        .hash
        .as_ref()
        .ok_or(anyhow!("no hash"))?;

    Ok((
        seed.clone(),
        total_currency.to_string(),
        ledger_hash.clone(),
    ))
}

pub async fn get_delegators(ledger_hash: &str) -> Result<Vec<LedgerAccount>> {
    let url = format!(
        "https://raw.githubusercontent.com/zkvalidator/mina-graphql-rs/master/data/epochs/{}.json",
        ledger_hash,
    );

    let ledger: Vec<LedgerAccountJson> =
        serde_json::from_slice(&reqwest::get(url).await?.bytes().await?.to_vec())?;

    let delegators = extract_delegators(&ledger);

    Ok(delegators)
}

pub async fn get_staking_data_from_mina_explorer(
    epoch: i64,
) -> Result<(String, String, String, Vec<LedgerAccount>)> {
    let (seed, total_currency, ledger_hash) =
        get_partial_staking_data_from_mina_explorer(epoch).await?;
    let delegators = get_delegators(&ledger_hash).await?;

    Ok((seed, total_currency, ledger_hash, delegators))
}

fn extract_delegators(ledger: &[LedgerAccountJson]) -> Vec<LedgerAccount> {
    let delegators = ledger
        .into_iter()
        .enumerate()
        .map(|(i, a)| LedgerAccount {
            pk: a.pk.clone(),
            balance: a.balance.clone(),
            delegate: a.delegate.clone(),
            index: i as i64,
        })
        .collect();

    delegators
}

pub async fn get_staking_percentages(public_key: &str, epoch: i64) -> Result<Vec<Delegator>> {
    let (_, _, _, delegators) = get_staking_data_from_mina_explorer(epoch).await?;

    let delegators_for_pk = delegators
        .into_iter()
        .filter(|d| &d.delegate == public_key)
        .collect::<Vec<_>>();
    let sum_delegations = delegators_for_pk
        .iter()
        .try_fold::<_, _, Result<Decimal>>(Decimal::new(0, 0), |sum, d| {
            Ok(sum + Decimal::from_str(&d.balance)?)
        })?;

    let delegators_with_percentages = delegators_for_pk
        .into_iter()
        .map(|d| {
            let balance = Decimal::from_str(&d.balance)?;
            let percentage = balance / sum_delegations;
            Ok(Delegator {
                pk: d.pk.clone(),
                balance: d.balance.clone(),
                percentage: percentage.to_string(),
            })
        })
        .collect::<Result<Vec<_>>>();

    delegators_with_percentages
}

pub async fn get_blocks_won(public_key: &str, epoch: i64) -> Result<Vec<i64>> {
    let request_body = BlocksWon::build_query(blocks_won::Variables {
        creator: public_key.to_string(),
        epoch,
        limit: NUM_SLOTS_IN_EPOCH as i64,
    });
    let data: blocks_won::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;

    let block_nums = data
        .blocks
        .into_iter()
        .map(|b| {
            b.ok_or(anyhow!("should have had block"))?
                .block_height
                .ok_or(anyhow!("should have had block height"))
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(block_nums)
}

pub async fn get_expected_rewards_for_epoch(public_key: &str, epoch: i64) -> Result<Decimal> {
    let blocks_won = get_blocks_won(public_key, epoch).await?;
    let rewards = Decimal::new(COINBASE_REWARD_NON_SUPERCHARGED, 0)
        * Decimal::new(blocks_won.len() as i64, 0);
    Ok(rewards)
}

pub async fn get_expected_payment_for_foundation_addresses(
    public_key: &str,
    epoch: i64,
    addresses: &[String],
) -> Result<HashMap<String, Decimal>> {
    let foundation_delegation_percentage_to_pay_back =
        Decimal::new(FOUNDATION_DELEGATION_PERCENTAGE_TO_PAY_BACK, 2);

    let mut payment_for_address = HashMap::new();
    for address in addresses {
        payment_for_address.insert(address.clone(), Decimal::new(0, 0));
    }

    let percentages = get_staking_percentages(public_key, epoch).await?;
    let reward_for_epoch = get_expected_rewards_for_epoch(public_key, epoch).await?;
    for address in addresses {
        let percentage = Decimal::from_str(
            &percentages
                .iter()
                .find(|p| &p.pk == address)
                .ok_or(anyhow!("should have found delegator"))?
                .percentage,
        )?;
        *payment_for_address
            .get_mut(address)
            .ok_or(anyhow!("should have gotten address in map"))? +=
            reward_for_epoch * percentage * foundation_delegation_percentage_to_pay_back;
    }

    Ok(payment_for_address)
}

pub async fn get_expected_payment_for_foundation_addresses_until_max_epoch(
    max_epoch: i64,
    public_key: &str,
    addresses: &[String],
) -> Result<HashMap<String, Decimal>> {
    let mut payment_for_address = HashMap::new();
    for address in addresses {
        payment_for_address.insert(address.clone(), Decimal::new(0, 0));
    }

    for epoch in 0..=max_epoch {
        let payment_for_address_for_epoch =
            get_expected_payment_for_foundation_addresses(public_key, epoch, addresses).await?;
        for address in addresses {
            *payment_for_address
                .get_mut(address)
                .ok_or(anyhow!("should have gotten address in map"))? +=
                payment_for_address_for_epoch
                    .get(address)
                    .ok_or(anyhow!("should have gotten address in map"))?;
        }
    }

    Ok(payment_for_address)
}

pub async fn get_payments_performed_to_addresses(
    addresses: &[String],
    senders: &[String],
) -> Result<HashMap<String, Decimal>> {
    let mut payments_performed_for_address = HashMap::new();
    for address in addresses {
        payments_performed_for_address.insert(address.clone(), Decimal::new(0, 0));
    }

    for from in senders {
        for to in addresses {
            let payments_performed = get_total_payments_performed_single(to, from).await?;
            *payments_performed_for_address
                .get_mut(to)
                .ok_or(anyhow!("should have gotten address in map"))? += payments_performed;
        }
    }

    Ok(payments_performed_for_address)
}

pub async fn get_total_payments_performed_single(to: &str, from: &str) -> Result<Decimal> {
    let request_body =
        TransactionsBetweenAddresses::build_query(transactions_between_addresses::Variables {
            from: from.to_string(),
            to: to.to_string(),
        });
    let data: transactions_between_addresses::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;
    let mut total_payments = Decimal::new(0, 0);
    for payment in data.transactions {
        let mut amount = Decimal::from_f64(
            payment
                .ok_or(anyhow!("should have had payment"))?
                .amount
                .ok_or(anyhow!("should have had amount"))?,
        )
        .ok_or(anyhow!("should have converted amount"))?;
        amount.set_scale(DIGITS_AFTER_DECIMAL_POINT)?;
        total_payments += amount;
    }

    Ok(total_payments)
}

pub async fn get_last_block_in_epoch(epoch: i64) -> Result<i64> {
    let request_body = LastBlockInEpoch::build_query(last_block_in_epoch::Variables { epoch });
    let data: last_block_in_epoch::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;
    let block_height = data.blocks[0]
        .as_ref()
        .ok_or(anyhow!("no block"))?
        .protocol_state
        .as_ref()
        .ok_or(anyhow!("no protocol state"))?
        .consensus_state
        .as_ref()
        .ok_or(anyhow!("no consensus state"))?
        .block_height
        .ok_or(anyhow!("no block height"))?;

    Ok(block_height)
}

pub async fn verify_enough_payments(
    max_epoch: i64,
    public_key: &str,
    addresses: &[String],
    senders: &[String],
) -> Result<HashMap<String, DelegatorPayment>> {
    let mut payments_diffs = HashMap::new();
    let payments_expected = get_expected_payment_for_foundation_addresses_until_max_epoch(
        max_epoch, public_key, addresses,
    )
    .await?;
    let payments_performed = get_payments_performed_to_addresses(addresses, senders).await?;
    for address in addresses {
        let payment_expected = payments_expected
            .get(address)
            .ok_or(anyhow!("should have found address in map"))?;
        let payment_performed = payments_performed
            .get(address)
            .ok_or(anyhow!("should have found address in map"))?;
        payments_diffs.insert(
            address.clone(),
            DelegatorPayment {
                expected: payment_expected.clone(),
                performed: payment_performed.clone(),
                diff: payment_expected - payment_performed,
            },
        );
    }

    Ok(payments_diffs)
}

pub async fn get_epoch_blocks_winners_from_explorer(
    epoch: i64,
) -> Result<Vec<epoch_blocks_winners::EpochBlocksWinnersBlocks>> {
    let request_body = EpochBlocksWinners::build_query(epoch_blocks_winners::Variables { epoch });
    let data: epoch_blocks_winners::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;
    let mut blocks = vec![];
    for b in data.blocks {
        if b.is_none() {
            continue;
        };
        blocks.push(b.unwrap());
    }
    Ok(blocks)
}

pub async fn get_epoch_blocks_for_creator_from_explorer(
    epoch: i64,
    public_key: &str,
) -> Result<Vec<epoch_blocks_for_creator::EpochBlocksForCreatorBlocks>> {
    let request_body = EpochBlocksForCreator::build_query(epoch_blocks_for_creator::Variables {
        epoch,
        creator: public_key.to_string(),
    });
    let data: epoch_blocks_for_creator::ResponseData =
        graphql_query(MINA_EXPLORER_ENDPOINT, &request_body).await?;
    let mut blocks = vec![];
    for b in data.blocks {
        if b.is_none() {
            continue;
        };
        blocks.push(b.unwrap());
    }
    Ok(blocks)
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct BlockResult {
    pub vrf: String,
    pub block_height: i64,
    pub date_time: DateTime<FixedOffset>,
    pub public_key: String,
    pub received_time: DateTime<FixedOffset>,
}

pub async fn get_winners_for_epoch(epoch: usize) -> Result<HashMap<i64, BlockResult>> {
    let blocks = get_epoch_blocks_winners_from_explorer(epoch as i64).await?;
    let mut winner_result: HashMap<i64, BlockResult> = HashMap::new();

    for b in blocks {
        let consensus_state = b
            .protocol_state
            .as_ref()
            .ok_or(anyhow!("no protocol state"))?
            .consensus_state
            .as_ref()
            .ok_or(anyhow!("no consensus state"))?;

        let slot = consensus_state
            .slot_since_genesis
            .ok_or(anyhow!("couldn't get global slot"))?;

        let block_height = b.block_height.ok_or(anyhow!("couldn't get block height"))?;
        let date_time =
            DateTime::parse_from_rfc3339(&b.date_time.ok_or(anyhow!("couldn't get slot time"))?)?;
        let received_time = DateTime::parse_from_rfc3339(
            &b.received_time.ok_or(anyhow!("couldn't get received"))?,
        )?;

        let winner = b
            .winner_account
            .as_ref()
            .ok_or(anyhow!("no winner_account"))?
            .public_key
            .as_ref()
            .ok_or(anyhow!("winner_account no public_key"))?;

        let vrf = consensus_state
            .last_vrf_output
            .as_ref()
            .ok_or(anyhow!("no vrf"))?;

        winner_result.insert(
            slot,
            BlockResult {
                public_key: winner.to_string(),
                block_height,
                vrf: vrf.to_string(),
                date_time,
                received_time,
            },
        );
    }

    Ok(winner_result)
}

pub async fn get_blocks_for_creator_for_epoch(
    epoch: usize,
    public_key: &str,
) -> Result<HashMap<i64, BlockResult>> {
    let blocks = get_epoch_blocks_for_creator_from_explorer(epoch as i64, public_key).await?;
    let mut winner_result: HashMap<i64, BlockResult> = HashMap::new();

    for b in blocks {
        let consensus_state = b
            .protocol_state
            .as_ref()
            .ok_or(anyhow!("no protocol state"))?
            .consensus_state
            .as_ref()
            .ok_or(anyhow!("no consensus state"))?;

        let slot = consensus_state
            .slot_since_genesis
            .ok_or(anyhow!("couldn't get global slot"))?;

        let block_height = b.block_height.ok_or(anyhow!("couldn't get block height"))?;
        let date_time =
            DateTime::parse_from_rfc3339(&b.date_time.ok_or(anyhow!("couldn't get slot time"))?)?;
        let received_time = DateTime::parse_from_rfc3339(
            &b.received_time.ok_or(anyhow!("couldn't get received"))?,
        )?;

        let winner = b
            .winner_account
            .as_ref()
            .ok_or(anyhow!("no winner_account"))?
            .public_key
            .as_ref()
            .ok_or(anyhow!("winner_account no public_key"))?;

        let vrf = consensus_state
            .last_vrf_output
            .as_ref()
            .ok_or(anyhow!("no vrf"))?;

        winner_result.insert(
            slot,
            BlockResult {
                public_key: winner.to_string(),
                block_height,
                vrf: vrf.to_string(),
                date_time,
                received_time,
            },
        );
    }

    Ok(winner_result)
}
