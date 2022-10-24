import type { ReactNode } from "react";
import type { Network } from "./store";
import type { RouteFilterType } from "./routes";
import type {
  BlockStatus,
  SignalEntity,
  SignalStatus,
  StatsEntity,
} from "./data";

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
  stats: StatsEntity;
}
export interface TableRowProps {
  signal: SignalEntity;
  stats: StatsEntity;
}

export interface TableNavElementProps {
  title: string;
  filter: RouteFilterType;
}

export interface TableBubbleProps extends ComponentWithChildren {
  status: BlockStatus | SignalStatus;
}

export interface ModalProps extends ComponentWithChildren {
  state: boolean;
  setState: (v: boolean) => void;
}

export interface StatsWeightedProps {
  stats: StatsEntity;
}
