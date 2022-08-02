import type { Network } from "./store";
import type { RouteFilterType } from "./routes";
import type { SignalEntity } from "./data";

export interface SettingsButtonProps {
  title: Network;
}

export interface TableNavElementProps {
  title: string;
  filter: RouteFilterType;
}

export interface TableRowProps {
  signal: SignalEntity;
}
