import axios from "axios";

import type { DataEntity, Network } from "@/types";

import { DEV_API_URL, PROD_API_URL } from "@/constants";

/**
 * Builds API URL for our keyword query.
 * @param {string[]} key
 * @param {string | null} filter
 * @param {Network} network
 */
const buildAPIUrl = (key: string, network: Network): string => {
  return `${import.meta.env.DEV ? DEV_API_URL : PROD_API_URL}/${key}?network=${
    network[0].toUpperCase() + network.slice(1).toLowerCase()
  }`;
};

/**
 * Requests keyword data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 * @param {Network} network
 */
export const fetchKeywordData = async (
  key: string | undefined,
  network: string | null,
) => {
  if (!key) {
    throw new Error("Please provide a valid key.");
  }

  const { data } = await axios.get(
    buildAPIUrl(key, network ? (network as Network) : "Mainnet"),
    {
      method: "GET",
      headers: {
        "Content-type": "application/json",
      },
    },
  );
  return data as DataEntity;
};
