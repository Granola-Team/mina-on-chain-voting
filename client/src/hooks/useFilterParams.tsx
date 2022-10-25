import { useSearchParams } from "react-router-dom";

import type { RoutesParams } from "@/types";

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
    const filter = searchParams.get("filter");

    const routes = {
      ...((filter || params.filter) && {
        filter: params.filter ? params.filter : filter!,
      }),
    };

    setSearchParams({ ...routes });
  };

  return [searchParams, executeRoute];
};
