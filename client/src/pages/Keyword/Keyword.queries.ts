import axios from "axios";

import type { EpochEntity, Network, SignalEntity } from "@/types";

import { DEV_API_URL, PROD_API_URL } from "@/constants";

/**
 * Builds API URL for our keyword query.
 * @param {string[]} key
 * @param {Network} network
 */
const buildAPIUrl = (
  key: string,
  network: Network,
  start: string,
  end: string,
  hash: string | null,
): string => {
  if (hash) {
    return `${
      import.meta.env.DEV ? DEV_API_URL : PROD_API_URL
    }/${key}/results?network=${network}&start=${start}&end=${end}&hash=${hash}`;
  }

  return `${
    import.meta.env.DEV ? DEV_API_URL : PROD_API_URL
  }/${key}?network=${network}&start=${start}&end=${end}`;
};

/**
 * Requests keyword data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 * @param {Network} network
 */
export const fetchKeywordData = async (
  key: string | undefined,
  network: Network,
  start: string,
  end: string,
  hash: string | null,
) => {
  if (!key) {
    throw new Error("Please provide a valid key.");
  }

  const { data } = await axios.get(
    buildAPIUrl(key, network, start, end, hash),
    {
      method: "GET",
      headers: {
        "Content-type": "application/json",
      },
    },
  );
  return data as SignalEntity[];
};

/**
 * Requests epoch data.
 */
export const fetchEpochData = async () => {
  const { data } = await axios.get("https://api.minaexplorer.com/summary", {
    method: "GET",
    headers: {
      "Content-type": "application/json",
    },
  });

  return data as EpochEntity;
};
