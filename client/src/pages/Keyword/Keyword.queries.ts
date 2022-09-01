import axios from "axios";

import type { DataEntity } from "@/types";

import { API_URL } from "@/constants";

/**
 * Builds API URL for our keyword query.
 * @param {string[]} key
 * @param {string | null} filter
 */
const buildAPIUrl = (key: string, filter: string | null): string => {
  return `${API_URL}/${key}?filter=${filter ?? "All"}`;
};

/**
 * Requests keyword data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 */
export const fetchKeywordData = async (
  key: string | undefined,
  filter: string | null,
) => {
  if (!key) {
    throw new Error("Please provide a valid key.");
  }

  const { data } = await axios.get(buildAPIUrl(key, filter), {
    method: "GET",
    headers: {
      "Content-type": "application/json",
    },
  });
  return data as DataEntity;
};
