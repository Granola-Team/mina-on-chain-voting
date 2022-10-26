export type BlockStatus = "Canonical" | "Orphaned" | "Pending";

export type SignalStatus = "Invalid" | "Settled" | "Unsettled";

export interface DelegationEntity {
  delegated_balance: string;
  total_delegators: number;
}

export interface SignalEntity {
  height: number;
  timestamp: number;
  account: string;
  memo: string;
  status: BlockStatus;
  signal_status: SignalStatus;
  delegations: DelegationEntity | null;
}

export interface StatsEntity {
  yes: number;
  no: number;
}

export interface DataEntity {
  settled: SignalEntity[];
  unsettled: SignalEntity[];
  invalid: SignalEntity[];
  stats: StatsEntity | null;
}

export interface EpochEntity {
  epoch: number;
  slot: number;
}
