export type BlockStatus = "Canonical" | "Orphaned" | "Pending";

export interface SignalEntity {
  account: string;
  hash: string;
  memo: string;
  height: number;
  timestamp: number;
  status: BlockStatus;
  stake_weight?: number;
}

export interface EpochEntity {
  epoch: number;
  slot: number;
}
