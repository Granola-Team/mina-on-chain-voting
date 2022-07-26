declare module '*.svg' {
    const content: any;
    export default content;
  }
  
  export type Status = 'Unsettled' | 'Settled';
  export type VoteEntry = { 
    account: string; 
    memo: string; 
    height: number; 
    timestamp: number; 
    valid: boolean; 
    blockstatus: string 
    };
  export type AccountEntry = { status: Status; results: VoteEntry[] };
  export type VoteCheckResult = 'for' | 'against' | 'invalid';
  