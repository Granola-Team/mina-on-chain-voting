import React, { useEffect } from "react";
import shallow from "zustand/shallow";
import { useParams } from "react-router-dom";
import { useQuery } from "@tanstack/react-query";

import { useFilterParams } from "@/hooks/useFilterParams";

import { Spinner } from "@/components/Spinner";
import { Layout } from "@/components/Layout";
import { StatsWeighted } from "@/components/Stats";
import { Table } from "@/components/Table";

import { useKeywordStore } from "./Keyword.store";
import { fetchKeywordData } from "./Keyword.queries";

import { unsortedData, settledData, unsettledData, invalidData } from "@/dummy";
import { useAppStore } from "@/App.store";

export const Keyword = () => {
  const { signals, setSignals, isLoading, setIsLoading } = useKeywordStore(
    (state) => ({
      signals: state.signals,
      setSignals: state.setSignals,
      isLoading: state.isLoading,
      setIsLoading: state.setIsLoading,
    }),
    shallow,
  );

  /**
   * Gets current route param.
   * @param {string} key - Route to control current keyword. (e.g. "/magenta")
   */
  const { key } = useParams();

  /**
   * Gets current search parameters.
   * @param {string} filter - Param to control filters. (e.g. 'Settled' only)
   * @param {string} demo - Param to toggle demonstration mode.
   */
  const [searchParams] = useFilterParams();
  const filter = searchParams.get("filter");
  const demo = searchParams.get("demo");
  const network = useAppStore((state) => state.network);

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
    [key, filter ?? "All", network],
    () => {
      setIsLoading(true);
      return fetchKeywordData(key, filter, network);
    },
    {
      enabled: !demo && !!key,
      onSuccess: () => {
        setIsLoading(false);
      },
    },
  );

  /**
   * Settings the result of our query to our local state.
   * @param {boolean} isSuccess - Denotes our query's success.
   * @param {any[]} queryData - Our query's data result.
   */
  useEffect(() => {
    if (isSuccess) {
      setSignals(queryData);
    }
  }, [queryData, isSuccess, setSignals]);

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
  }, [demo, key, filter, setSignals]);

  if (isError) {
    return (
      <Layout>
        <div className="mx-auto pt-10">Something went wrong.</div>
      </Layout>
    );
  }

  if (isLoading && !signals) {
    return (
      <Layout>
        <div className="py-8 mx-auto">
          <Spinner />
        </div>
      </Layout>
    );
  }

  if ((signals && signals.stats && key) || (signals && signals.stats && demo)) {
    return (
      <Layout>
        <React.Fragment>
          <StatsWeighted stats={signals.stats} />
          <Table data={signals} query={key} isLoading={isLoading} />
        </React.Fragment>
      </Layout>
    );
  }

  return null;
};
