export type BlockStatus = "Canonical" | "Pending" | "Orphaned";

export type SignalStatus = "Settled" | "Unsettled" | "Invalid";

export interface SignalEntity {
  height: number;
  timestamp: number;
  account: string;
  memo: string;
  status: BlockStatus;
  signal_status: SignalStatus;
}

export interface DataEntity {
  signals: SignalEntity[];
}
