import { z } from 'zod';

export const VoteDirections = ['FOR', 'AGAINST'] as const;
export const VoteDirectionSchema = z.enum(VoteDirections);
export type VoteDirection = (typeof VoteDirections)[number];

export const VoteStatuses = ['Pending', 'Canonical', 'Orphaned'] as const;
export const VoteStatusSchema = z.enum(VoteStatuses);
export type VoteStatus = (typeof VoteStatuses)[number];

export const VoteMetricsSchema = z.object({
  DATE: z.string(),
  FOR: z.number(),
  AGAINST: z.number(),
});
export type VoteMetrics = z.infer<typeof VoteMetricsSchema>;

export const VoteSchema = z.object({
  account: z.string(),
  hash: z.string(),
  memo: z.string(),
  height: z.number(),
  status: VoteStatusSchema,
  timestamp: z.number(),
  nonce: z.number(),
});

export const VoteWithWeightSchema = VoteSchema.and(
  z.object({
    weight: z.coerce.number(),
  })
);

export const ProposalCategories = ['Core', 'Networking', 'Interface', 'ERC', 'Cryptography'] as const;
export const ProposalCategorySchema = z.enum(ProposalCategories);
export type ProposalCategory = (typeof ProposalCategories)[number];

export const ProposalStatuses = ['Pending', 'In Progress', 'In Review', 'Completed', 'Unknown'] as const;
export const ProposalStatusSchema = z.enum(ProposalStatuses);
export type ProposalStatus = z.infer<typeof ProposalStatusSchema>;

export const ProposalSchema = z.object({
  id: z.number(),
  key: z.string(),
  start_time: z.number(),
  end_time: z.number(),
  ledger_hash: z.string().nullable(),
  category: ProposalCategorySchema,
  version: z.string(),
  title: z.string(),
  description: z.string(),
  url: z.string(),
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

export const getProposalsSchema = z.array(ProposalSchema);
