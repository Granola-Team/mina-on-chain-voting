import axios from "axios";

import { API_PROD_URL, API_DEV_URL } from "@/constant";
import { isDev } from "@/utils/devMode";

const buildAPIUrl = (key: string | null, filter: string | null): string => {
  return `${isDev() ? API_DEV_URL : API_PROD_URL}/${
    key ? key : "magenta"
  }?filter=${filter ? filter : "All"}`;
};

export const fetchKeywordData = async (key: string, filter: string) => {
  const { data } = await axios.get(buildAPIUrl(key, filter));
  return data;
};
