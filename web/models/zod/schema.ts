import { z } from 'zod';

export const minaBlockStatusSchema = z.union([z.literal('Pending'), z.literal('Canonical'), z.literal('Orphaned')]);

export const minaVoteSchema = z.object({
  account: z.string(),
  hash: z.string(),
  memo: z.string(),
  height: z.number(),
  status: minaBlockStatusSchema,
  timestamp: z.number(),
  nonce: z.number(),
  weight: z.string().nullable(),
});

export const minaProposalSchema = z.object({
  id: z.number(),
  key: z.string(),
  global_start_slot: z.number(),
  global_end_slot: z.number(),
  ledger_hash: z.string().nullable(),
});

export const getMinaProposalResponseSchema = minaProposalSchema.and(
  z.object({
    votes: z.array(minaVoteSchema),
  })
);
