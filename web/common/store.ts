import { atom, useAtom } from 'jotai';
import { atomsWithQuery } from 'jotai-tanstack-query';
import { MinaProposalParser } from 'models';

import { safeFetch } from './fetch';

export enum QueryKeys {
  MINA_PROPOSAL = 'mina-proposal',
  MINA_PROPOSAL_RESULT = 'mina-proposal-result',
}

export const minaProposalIdAtom = atom(0);
const [minaProposalAtom] = atomsWithQuery((get) => ({
  queryKey: [QueryKeys.MINA_PROPOSAL, get(minaProposalIdAtom)],
  queryFn: async ({ queryKey: [, id] }) => {
    const res = await safeFetch(`/api/proposal/${id}`);
    const data = MinaProposalParser.parse(QueryKeys.MINA_PROPOSAL, res);
    return data;
  },
}));

const [minaProposalResultsAtom] = atomsWithQuery((get) => ({
  queryKey: [QueryKeys.MINA_PROPOSAL_RESULT, get(minaProposalIdAtom)],
  queryFn: async ({ queryKey: [, id] }) => {
    const res = await safeFetch(`/api/proposal/${id}/results`);
    const data = MinaProposalParser.parse(QueryKeys.MINA_PROPOSAL_RESULT, res);
    return data;
  },
}));

export const useMinaProposal = () => useAtom(minaProposalAtom);
export const useMinaProposalResults = () => useAtom(minaProposalResultsAtom);
