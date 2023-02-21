//! Typeshare currently does not support serde derive annotations.
//! This leaves us having to declare our versions of our types multiple times.
//! Described in issue: https://github.com/1Password/typeshare/issues/65
//! Until this issue is solved - these typings will act as stand-in.

export type MinaBlockStatus = 'Pending' | 'Canonical' | 'Orphaned';

export type MinaVote = {
  account: string;
  hash: string;
  memo: string;
  height: bigint;
  status: MinaBlockStatus;
  timestamp: bigint;
  nonce: bigint;
  weight?: string;
};

export type MinaProposal = {
  id: number;
  key: string;
  global_start_slot: number;
  global_end_slot: number;
  ledger_hash?: string;
};

export type GetMinaProposalResponse = MinaProposal & {
  votes: MinaVote[];
};

export type GetMinaProposalResultsResponse = MinaProposal & {
  votes: MinaVote[];
};
