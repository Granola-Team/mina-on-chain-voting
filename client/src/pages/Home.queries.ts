import axios from "axios";

import { API_URL } from "@/constant";

/**
 * Builds API URL for a query.
 * @param {string[]} key
 * @param {string | null} filter
 */
const buildAPIUrl = (key: string, filter: string | null): string => {
  return `${API_URL}/${key}?filter=${filter ? filter : "All"}&sorted=true`;
};

/**
 * Requests data from our API & returns said data.
 * @param {string} key
 * @param {string} filter
 */
export const fetchKeywordData = async (
  key: string | null,
  filter: string | null,
) => {
  if (key) {
    const { data } = await axios.get(buildAPIUrl(key, filter));
    return data;
  }
};
