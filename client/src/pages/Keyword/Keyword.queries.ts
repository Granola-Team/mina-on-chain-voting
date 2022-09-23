import axios from "axios";

import type { DataEntity, Network } from "@/types";

import { DEV_API_URL, PROD_API_URL } from "@/constants";

/**
 * Builds API URL for our keyword query.
 * @param {string[]} key
 * @param {string | null} filter
 * @param {Network} network
 */
const buildAPIUrl = (
  key: string,
  filter: string | null,
  network: Network,
): string => {
  return `${import.meta.env.DEV ? DEV_API_URL : PROD_API_URL}/${key}?filter=${
    filter ?? "All"
  }&network=${network}`;
};

/**
 * Requests keyword data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 * @param {Network} network
 */
export const fetchKeywordData = async (
  key: string | undefined,
  filter: string | null,
  network: Network,
) => {
  if (!key) {
    throw new Error("Please provide a valid key.");
  }

  const { data } = await axios.get(buildAPIUrl(key, filter, network), {
    method: "GET",
    headers: {
      "Content-type": "application/json",
    },
  });
  return data as DataEntity;
};
