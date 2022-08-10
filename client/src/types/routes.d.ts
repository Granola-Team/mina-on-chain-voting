export type RouteFilterType = "All" | "Invalid" | "Settled" | "Unsettled";

export interface RoutesParams {
  filter?: RouteFilterType;
  demo?: string;
}
