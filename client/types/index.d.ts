declare module '*.svg' {
  const content: any;
  export default content;
}

export type Status = 'Pending' | 'Canonical';
export type VoteEntry = { memo: string; height: number; status: Status };
export type AccountEntry = { account: string; votes: VoteEntry[] };
export type VoteCheckResult = 'for' | 'against' | 'invalid';
