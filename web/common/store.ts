import { atom, useAtom } from 'jotai';
import { atomsWithQuery } from 'jotai-tanstack-query';
import { CoreApiInfoParser, ProposalListParser, ProposalParser, ProposalResultsParser } from 'models';

import { safeFetch } from './fetch';

export enum QueryKeys {
  INFO = 'info',
  PROPOSAL = 'proposal',
  PROPOSAL_LIST = 'proposal-list',
  PROPOSAL_RESULT = 'proposal-result',
}

const [coreApiInfoAtom] = atomsWithQuery(() => ({
  queryKey: [QueryKeys.INFO],
  staleTime: 60 * 60 * 5,
  queryFn: async () => {
    const res = await safeFetch('/api/info');
    const data = CoreApiInfoParser.parse(QueryKeys.INFO, res);
    return data;
  },
}));

export const proposalIdAtom = atom(0);

const [proposalAtom] = atomsWithQuery((get) => ({
  queryKey: [QueryKeys.PROPOSAL, get(proposalIdAtom)],
  queryFn: async ({ queryKey: [, id] }) => {
    const res = await safeFetch(`/api/proposal/${id}`);
    const data = ProposalParser.parse(QueryKeys.PROPOSAL, res);
    return data;
  },
}));

const [proposalResultsAtom] = atomsWithQuery((get) => ({
  queryKey: [QueryKeys.PROPOSAL_RESULT, get(proposalIdAtom)],
  queryFn: async ({ queryKey: [, id] }) => {
    const res = await safeFetch(`/api/proposal/${id}/results`);
    const data = ProposalResultsParser.parse(QueryKeys.PROPOSAL_RESULT, res);
    return data;
  },
}));

const [proposalListAtom] = atomsWithQuery(() => ({
  queryKey: [QueryKeys.PROPOSAL],
  queryFn: async () => {
    const res = await safeFetch('/api/proposals');
    const data = ProposalListParser.parse(QueryKeys.PROPOSAL, res);
    return data;
  },
}));

export const useCoreApiInfo = () => useAtom(coreApiInfoAtom);

export const useProposal = () => useAtom(proposalAtom);
export const useProposalResults = () => useAtom(proposalResultsAtom);
export const useProposalList = () => useAtom(proposalListAtom);
