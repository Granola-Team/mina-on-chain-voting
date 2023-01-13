import type { ReactNode } from "react";
import type { Network } from "./store";
import type { BlockStatus, SignalEntity } from "./data";

export interface ComponentWithChildren {
  children: ReactNode;
}

export interface SettingsButtonProps {
  title: Network;
}

export interface TableProps {
  data: SignalEntity[];
  query: string | undefined;
  isLoading: boolean;
}

export interface SliceTableProps extends TableProps {
  rowsPerPage: number;
}

export interface TableRowProps {
  signal: SignalEntity;
}

export interface TableNavElementProps {
  title: string;
}

export interface TableBubbleProps extends ComponentWithChildren {
  status: BlockStatus;
}

export interface ModalProps extends ComponentWithChildren {
  state: boolean;
  setState: (v: boolean) => void;
}

export interface StatsWeightedProps {
  network: string;
}
