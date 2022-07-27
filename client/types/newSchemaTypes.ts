/*
declare module '*.svg' {
    const content: any;
    export default content;
  }
*/  
  export type Status = 'Unsettled' | 'Settled' | 'Invalid';
  export type VoteEntry = { 
    account: string; 
    memo: string; 
    height: number; 
    timestamp: number;  
    blockstatus: string 
    };
  export type AccountEntry = { status: Status; results: VoteEntry[] };
  export type VoteCheckResult = 'for' | 'against' | 'invalid';
  