export type BlockStatus = "Canonical" | "Orphaned" | "Pending";

export type SignalStatus = "Invalid" | "Settled" | "Unsettled";

export interface SignalEntity {
  height: number;
  timestamp: number;
  account: string;
  memo: string;
  status: BlockStatus;
  signal_status: SignalStatus;
}

export interface StatsEntity {
  yes: number;
  no: number;
}

export interface DataEntity {
  signals: SignalEntity[];
  stats: StatsEntity | null;
}
