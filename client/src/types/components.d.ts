import type { Network } from "./store";
import type { FilterType } from "./routes";

export interface SettingsButtonProps {
  title: Network;
}

export interface TableNavElementProps {
  title: string;
  filter: FilterType;
}
