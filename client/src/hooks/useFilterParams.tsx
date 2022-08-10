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
    const demo = searchParams.get("demo");

    const routes = {
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
