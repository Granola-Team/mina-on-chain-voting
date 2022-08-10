import React, { useState, useEffect } from "react";
import { useQuery } from "@tanstack/react-query";

import type { DataEntity } from "@/types";

import { useFilterParams } from "@/hooks/useFilterParams";

import { Layout } from "@/components/Layout";
import { StatsWeighted } from "@/components/Stats";
import { Table } from "@/components/Table";

import { useAppStore } from "@/store/app.store";
import { fetchKeywordData } from "./Home.queries";

import { unsortedData, settledData, unsettledData, invalidData } from "@/dummy";

// TODO! REWORK LOADING LOGIC

export const Home = () => {
  const [signals, setSignals] = useState<DataEntity>();
  const isFetching = useAppStore((state) => state.isFetching);
  const setIsFetching = useAppStore((state) => state.setIsFetching);

  /**
   * Gets current search parameters.
   * @param {string} key - Keyword used in search. (e.g. 'magenta')
   * @param {string} filter - Param to control filters. (e.g. 'Settled' only)
   * @param {string} demo - Param to toggle demonstration mode.
   */
  const [searchParams] = useFilterParams();
  const key = searchParams.get("key");
  const filter = searchParams.get("filter");
  const demo = searchParams.get("demo");

  /**
   * Executing our query using React Query.
   * @restriction - Only executing the query if demo is false & we have a key.
   * @param {boolean} enabled - Param to compute if our query is active or not.
   */
  const {
    data: queryData,
    isSuccess,
    isError,
  } = useQuery(
    [key, filter ? filter : "All"],
    () => {
      setIsFetching(true);
      return fetchKeywordData(key, filter);
    },
    { enabled: !demo && !!key },
  );

  /**
   * Settings the result of our query to our local state.
   * @param {boolean} isSuccess - Denotes our query's success.
   * @param {any[]} queryData - Our query's data result.
   */
  useEffect(() => {
    if (isSuccess) {
      setSignals(queryData);
      setIsFetching(false);
    }
  }, [queryData, isSuccess]);

  /**
   * If in demonstration mode e.g. 'http://localhost:3000/?demo=true':
   * We'll only serve dummy data.
   */
  useEffect(() => {
    if (demo === "true") {
      switch (filter) {
        case "All":
          setSignals(unsortedData);
          break;
        case "Settled":
          setSignals(settledData);
          break;
        case "Unsettled":
          setSignals(unsettledData);
          break;
        case "Invalid":
          setSignals(invalidData);
          break;
        default:
          setSignals(unsortedData);
          break;
      }
    }
  }, [demo, key, filter]);

  if (isError) {
    return (
      <Layout>
        <div className="mx-auto pt-10">Something went wrong.</div>
      </Layout>
    );
  }

  // TODO! Rework conditional rendering.

  return (
    <Layout>
      {(signals && signals.stats && key) ||
      (signals && signals.stats && demo) ? (
        <React.Fragment>
          <StatsWeighted stats={signals.stats} />
          <Table data={signals} />
        </React.Fragment>
      ) : (
        <div className="mx-auto pt-10">
          {isFetching ? (
            <svg
              className="animate-spin h-8 w-8 text-white"
              xmlns="http://www.w3.org/2000/svg"
              fill="none"
              viewBox="0 0 24 24"
            >
              <circle
                className="opacity-25"
                cx="12"
                cy="12"
                r="10"
                stroke="currentColor"
                strokeWidth="4"
              />
              <path
                className="opacity-75"
                fill="currentColor"
                d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
              />
            </svg>
          ) : (
            <span>Please input a keyword.</span>
          )}
        </div>
      )}
    </Layout>
  );
};
