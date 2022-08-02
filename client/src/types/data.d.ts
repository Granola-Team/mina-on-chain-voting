export type BlockStatus = "Canonical" | "Pending" | "Orphaned";

export type SignalStatus = "Settled" | "Unsettled" | "Invalid";

export interface SignalEntity {
  height: number;
  timestamp: number;
  account: string;
  memo: string;
  block_status: BlockStatus;
  signal_status: SignalStatus;
}

export interface DataEntity {
  status: SignalStatus;
  results: SignalEntity[];
}
