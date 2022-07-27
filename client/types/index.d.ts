declare module '*.svg' {
  const content: any;
  export default content;
}

export type BlockStatus = 'Canonical' | 'Pending' | 'Orphaned'
export type Status = 'Unsettled' | 'Settled' | 'Invalid';
export type VoteEntry = { 
  account: string; 
  memo: string; 
  height: number; 
  blockstatus: BlockStatus; 
  timestamp: number;  
  };
export type AccountEntry = { status: Status; results: VoteEntry[] };
export type VoteCheckResult = 'for' | 'against' | 'invalid';
