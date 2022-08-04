import { ReactQueryClient } from "./client";

// TODO! FIX TYPING

export const prefetchQuery = async (
  queryKey: string[],
  queryArgs: any,
  fetchQuery: (args: any) => Promise<any>,
) => {
  await ReactQueryClient.prefetchQuery(queryKey, () => fetchQuery(queryArgs));
};
