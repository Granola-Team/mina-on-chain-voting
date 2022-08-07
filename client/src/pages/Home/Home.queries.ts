import axios from "axios";

import type { DataEntity } from "@/types";

import { API_URL } from "@/constant";

/**
 * Builds API URL for our keyword query.
 * @param {string[]} key
 * @param {string | null} filter
 */
const buildAPIUrl = (key: string, filter: string | null): string => {
  return `${API_URL}/${key}?filter=${filter ? filter : "All"}&sorted=true`;
};

/**
 * Requests keyword data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 */
export const fetchKeywordData = async (
  key: string | null,
  filter: string | null,
) => {
  if (!key) {
    throw new Error("Please provide a valid key.");
  }

  const { data } = await axios.get(buildAPIUrl(key, filter));
  return data as DataEntity;
};
