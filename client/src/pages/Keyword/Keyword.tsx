import React, { useEffect } from "react";
import { useParams } from "react-router-dom";
import { useQuery } from "@tanstack/react-query";

import shallow from "zustand/shallow";
import { useKeywordStore } from "./Keyword.store";
import { useFilterParams } from "@/hooks/useFilterParams";
import { fetchEpochData, fetchKeywordData } from "./Keyword.queries";

import { Instructions } from "@/components/Instructions";
import { EpochTiming } from "@/components/EpochTiming";
import { StatsWeighted } from "@/components/Stats";
import { Spinner } from "@/components/Spinner";
import { Layout } from "@/components/Layout";
import { Table } from "@/components/Table";

export const Keyword = () => {
  const {
    setKey,
    signals,
    setSignals,
    stats,
    setStats,
    isLoading,
    setIsLoading,
    timing,
    setTiming,
  } = useKeywordStore(
    (state) => ({
      setKey: state.setKey,
      signals: state.signals,
      setSignals: state.setSignals,
      stats: state.stats,
      setStats: state.setStats,
      isLoading: state.isLoading,
      setIsLoading: state.setIsLoading,
      timing: state.timing,
      setTiming: state.setTiming,
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
  const [searchParams] = useFilterParams(); // can we remove this?

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
    async () => {
      setIsLoading(true);
      const epochData = await fetchEpochData();
      const keywordData = await fetchKeywordData(
        key,
        network ? network : "mainnet",
      );
      return { ...epochData, ...keywordData };
    },
    {
      enabled: !!key,
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
    if (isSuccess && key) {
      setKey(key);
      setSignals(
        queryData.settled.concat(queryData.unsettled, queryData.invalid),
      );
      setStats(queryData.stats);
      setTiming({ epoch: queryData.epoch, slot: queryData.slot });
}
  }, [
    queryData,
    isSuccess,
    setSignals,
    setStats,
    setTiming,
    setKey,
    key,
  ]);

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

  if (signals && key && timing.epoch && timing.slot) {
    return (
      <Layout>
        <React.Fragment>
          <Instructions key={key} />
          {network === "mainnet" ? (
            <EpochTiming epoch={timing.epoch} slot={timing.slot} />
          ) : null}
          {stats ? (
            <StatsWeighted stats={stats} network={network ? network : ""} />
          ) : null}
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
