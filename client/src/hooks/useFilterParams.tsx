import { useSearchParams } from "react-router-dom";

import type { RoutesParams } from "@/types";

export const useFilterParams = (): [
  URLSearchParams,
  (params: RoutesParams) => void,
] => {
  const [searchParams, setSearchParams] = useSearchParams();

  const executeRoute = (params: RoutesParams): void => {
    const key = searchParams.get("key");
    const filter = searchParams.get("filter");

    const routes = {
      ...((key || params.key) && { key: params.key ? params.key : key! }),
      ...((filter || params.filter) && {
        filter: params.filter ? params.filter : filter!,
      }),
    };

    setSearchParams({ ...routes });
  };

  return [searchParams, executeRoute];
};
