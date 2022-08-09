import { useSearchParams } from "react-router-dom";

import type { RoutesParams } from "@/types";

// TODO: Rework routing logic.

/**
 * Custom hook that manages current search params & returns a function to execute computed route.
 * @returns [searchParams, executeRoute]
 */
export const useFilterParams = (): [
  URLSearchParams,
  (params: RoutesParams) => void,
] => {
  const [searchParams, setSearchParams] = useSearchParams();

  const executeRoute = (params: RoutesParams): void => {
    const key = searchParams.get("key");
    const filter = searchParams.get("filter");
    const demo = searchParams.get("demo");

    const routes = {
      ...((key || params.key) && { key: params.key ? params.key : key! }),
      ...((filter || params.filter) && {
        filter: params.filter ? params.filter : filter!,
      }),
      ...((demo || params.demo) && {
        demo: params.demo ? params.demo : demo!,
      }),
    };

    setSearchParams({ ...routes });
  };

  return [searchParams, executeRoute];
};
