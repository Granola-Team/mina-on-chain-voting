export type RouteFilterType = "all" | "settled" | "unsettled" | "invalid";

export interface RoutesParams {
  key?: string;
  filter?: RouteFilterType;
}
