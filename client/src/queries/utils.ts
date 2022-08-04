import { ReactQueryClient } from "./client";

// TODO!
//! Fix typing
//! Fix documentation
//! Fix function

/**
 * Prefetches a query & caches result in our query client.
 * @param {string[]} queryKey
 * @param {any} queryArgs
 */
export const prefetchQuery = async (
  queryKey: string[],
  queryArgs: any,
  fetchQuery: (args: any) => Promise<any>,
) => {
  await ReactQueryClient.prefetchQuery(queryKey, () => fetchQuery(queryArgs));
};
