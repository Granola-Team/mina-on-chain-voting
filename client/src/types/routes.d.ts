export type RouteFilterType = "All" | "Invalid" | "Settled" | "Unsettled";

export interface RoutesParams {
  key?: string;
  filter?: RouteFilterType;
  demo?: string;
}
