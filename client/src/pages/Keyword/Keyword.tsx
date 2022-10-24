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

export const Keyword = () => {
  const { signals, setSignals, stats, setStats, isLoading, setIsLoading } =
    useKeywordStore(
      (state) => ({
        signals: state.signals,
        setSignals: state.setSignals,
        stats: state.stats,
        setStats: state.setStats,
        isLoading: state.isLoading,
        setIsLoading: state.setIsLoading,
      }),
      shallow,
    );

  /**
   * Gets current route param.
   * @param {string} network - Route to control current network. (e.g. "/mainnet")
   * @param {string} key - Route to control current keyword. (e.g. "/magenta")
   */
  const { key, network } = useParams();

  /**
   * Gets current search parameters.
   * @param {string} filter - Param to control filters. (e.g. 'Settled' only)
   * @param {string} demo - Param to toggle demonstration mode.
   * @param {string} network - Param to control network. (e.g. Mainnet)
   */
  const [searchParams] = useFilterParams();
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
    [key, network],
    () => {
      setIsLoading(true);
      return fetchKeywordData(key, network ? network : "mainnet");
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
      switch (filter) {
        case "All":
          setSignals(
            queryData.settled.concat(queryData.unsettled, queryData.invalid),
          );
          setStats(queryData.stats);
          break;
        case "Settled":
          setSignals(queryData.settled);
          setStats(queryData.stats);
          break;
        case "Unsettled":
          setSignals(queryData.unsettled);
          setStats(queryData.stats);
          break;
        case "Invalid":
          setSignals(queryData.invalid);
          setStats(queryData.stats);
          break;
        default:
          setSignals(
            queryData.settled.concat(queryData.unsettled, queryData.invalid),
          );
          setStats(queryData.stats);
          break;
      }
    }
  }, [queryData, isSuccess, setSignals, filter, setStats]);

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

  if ((signals && stats && key) || (signals && stats && demo)) {
    return (
      <Layout>
        <React.Fragment>
          <StatsWeighted stats={stats} />
          <Table
            data={signals}
            stats={stats}
            query={key}
            isLoading={isLoading}
          />
        </React.Fragment>
      </Layout>
    );
  }

  return null;
};
