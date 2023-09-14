import { isEmpty } from 'common/utils';

import {
  CoreApiInfoParser,
  ProposalListParser,
  ProposalListParserOutcome,
  ProposalParser,
  ProposalParserOutcome,
  ProposalResultsParser,
  ProposalStatus,
  VoteDirection,
  VoteMetrics,
} from 'models';
import moment from 'moment';

import { safeFetch } from './fetch';

export enum QueryKeys {
  INFO = 'info',
  PROPOSAL = 'proposal',
  PROPOSAL_LIST = 'proposal-list',
  PROPOSAL_RESULT = 'proposal-result',
}

export const getCoreApiInfo = async () => {
  const res = await safeFetch('/api/info');
  const data = CoreApiInfoParser.parse(QueryKeys.INFO, res);
  return data;
};

export type GetCoreApiInfoResult = Awaited<ReturnType<typeof getCoreApiInfo>>;

export const getProposal = async (id: number | string) => {
  const res = await safeFetch(`/api/proposal/${id}`);
  const data = ProposalParser.parse(QueryKeys.PROPOSAL, res);
  const processedProposal = processProposal(data);
  const processedVotes = processVotes(data.votes);

  return {
    ...processedProposal,
    votes: processedVotes.votes,
    metrics: processedVotes.metrics,
  };
};

export type GetProposalResult = Awaited<ReturnType<typeof getProposal>>;

export const getProposalResults = async (id: number | string) => {
  const res = await safeFetch(`/api/proposal/${id}/results`);
  const data = ProposalResultsParser.parse(QueryKeys.PROPOSAL_RESULT, res);
  const processedProposal = processProposal(data);
  const processedVotes = processVotes(data.votes);

  return {
    ...processedProposal,
    votes: processedVotes.votes,
    metrics: processedVotes.metrics,
  };
};

export type GetProposalResultsResult = Awaited<ReturnType<typeof getProposalResults>>;

export const getProposalList = async () => {
  const res = await safeFetch('/api/proposals');
  const data = ProposalListParser.parse(QueryKeys.PROPOSAL, res);
  const processedProposals = data.map((row) => processProposal(row));
  return processedProposals;
};

export type GetProposalListResult = Awaited<ReturnType<typeof getProposalList>>;

// Function should be ported to the backend if needed, for consistency.
// i.e. expanding our backend models.
const processVotes = <T extends ProposalParserOutcome['votes'][number]>(
  votes: Array<T>
): {
  votes: Array<T & { direction: VoteDirection }>;
  metrics: Array<VoteMetrics>;
} => {
  const groupedVotes: { [date: string]: VoteMetrics } = {};
  const sortedVotes = [...votes].sort((a, b) => b.height - a.height);

  const processedVotes = sortedVotes.map((vote) => {
    const voteDate = moment(new Date(vote.timestamp)).utc().format('MMM DD');
    const isFor = !vote.memo.toLocaleLowerCase().includes('no');
    const direction = isFor ? 'FOR' : 'AGAINST';

    if (!groupedVotes[voteDate]) {
      groupedVotes[voteDate] = { FOR: 0, AGAINST: 0, DATE: voteDate };
    }

    groupedVotes[voteDate][direction] += 1;

    return {
      ...vote,
      direction: direction as VoteDirection,
    };
  });

  return {
    votes: processedVotes,
    metrics: Object.values(groupedVotes).reverse(),
  };
};

// Function should be ported to the backend if needed, for consistency.
// i.e. expanding our backend models.
const processProposal = <T extends ProposalListParserOutcome[number]>(
  proposal: T
): T & {
  status: ProposalStatus;
} => {
  const now = moment(new Date()).utc();
  const startDate = moment(new Date(proposal.start_time)).utc();
  const endDate = moment(new Date(proposal.end_time)).utc();

  const hasNotStarted = now.isBefore(startDate) && now.isBefore(endDate);
  const inProgress = now.isBetween(startDate, endDate);
  const inReview = now.isAfter(endDate) && isEmpty(proposal.ledger_hash);
  const isDone = now.isAfter(endDate) && !isEmpty(proposal.ledger_hash);

  return {
    ...proposal,
    status: hasNotStarted
      ? 'Pending'
      : inProgress
      ? 'In Progress'
      : inReview
      ? 'In Review'
      : isDone
      ? 'Completed'
      : 'Unknown',
  };
};
