import { z } from 'zod';

export const VoteSchema = z.object({
  account: z.string(),
  hash: z.string(),
  memo: z.string(),
  height: z.number(),
  status: z.union([z.literal('Pending'), z.literal('Canonical'), z.literal('Orphaned')]),
  timestamp: z.number(),
  nonce: z.number(),
});

export const VoteWithWeightSchema = VoteSchema.and(
  z.object({
    weight: z.coerce.number(),
  })
);

export const ProposalSchema = z.object({
  id: z.number(),
  key: z.string(),
  start_time: z.number(),
  end_time: z.number(),
  ledger_hash: z.string().nullable(),
});

export const getProposalSchema = ProposalSchema.and(
  z.object({
    votes: z.array(VoteSchema),
  })
);

export const getProposalResultsSchema = ProposalSchema.and(
  z.object({
    total_stake_weight: z.coerce.number(),
    positive_stake_weight: z.coerce.number(),
    negative_stake_weight: z.coerce.number(),
    votes: z.array(VoteWithWeightSchema),
  })
);

export const getCoreApiInfoResponseSchema = z.object({
  chain_tip: z.number(),
  current_slot: z.number(),
});
