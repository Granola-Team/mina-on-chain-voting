export type RouteFilterType = "all" | "invalid" | "settled" | "unsettled";

export interface RoutesParams {
  key?: string;
  filter?: RouteFilterType;
}
