import React, { useEffect } from "react";
import { useQuery } from "@tanstack/react-query";
import { useParams, useSearchParams } from "react-router-dom";

import shallow from "zustand/shallow";
import type { Network } from "@/types";
import { useKeywordStore } from "./Keyword.store";
import { fetchEpochData, fetchKeywordData } from "./Keyword.queries";

import { ResultsTable } from "@/components/ResultsTable";
import { Instructions } from "@/components/Instructions";
import { VotingPeriod } from "@/components/VotingPeriod";
import { StatsWeighted } from "@/components/Stats";
import { Spinner } from "@/components/Spinner";
import { Layout } from "@/components/Layout";
import { Table } from "@/components/Table";
import { ResultsOverview } from "@/components/ResultsOverview";

export const Keyword = ({ showResults }: { showResults: boolean }) => {
  const {
    setKey,
    signals,
    setSignals,
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
  const [searchParams] = useSearchParams();
  const start = searchParams.get("start");
  const end = searchParams.get("end");
  const hash = searchParams.get("hash");

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
        network ? (network as Network) : "mainnet",
        start ? start : "1665707162000",
        end ? end : "1670985755000",
        hash,
      );
      return { votes: keywordData, ...epochData };
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
      setSignals(queryData.votes);

      const yes = queryData.votes.reduce((acc, curr) => {
        if (
          curr.memo.toLowerCase() === key.toLowerCase() &&
          curr.stake_weight
        ) {
          return acc + curr.stake_weight;
        }
        return acc;
      }, 0);

      const no = queryData.votes.reduce((acc, curr) => {
        if (
          curr.memo.toLowerCase() === `no ${key.toLowerCase()}` &&
          curr.stake_weight
        ) {
          return acc + curr.stake_weight;
        }
        return acc;
      }, 0);

      setStats({ yes, no });
      setTiming({ epoch: queryData.epoch, slot: queryData.slot });
    }
  }, [queryData, isSuccess, setSignals, setStats, setTiming, setKey, key]);

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

  /** Display the Tracker Page */
  if (!showResults && start && end && signals && key) {
    return (
      <Layout>
        <React.Fragment>
          <Instructions key={key} totalVotes={signals.length} />

          {network === "mainnet" && timing.epoch && timing.slot ? (
            <VotingPeriod start={start} end={end} />
          ) : null}
          <Table data={signals} query={key} isLoading={isLoading} />
        </React.Fragment>
      </Layout>
    );
  }

  /** Display the Results Page */
  if (showResults && start && end && hash && signals && key) {
    return (
      <Layout>
        <React.Fragment>
          <ResultsOverview totalVotes={signals.length} />
          {network === "mainnet" && timing.epoch && timing.slot ? (
            <VotingPeriod start={start} end={end} />
          ) : null}
          <StatsWeighted network={network ? network : ""} />
          <ResultsTable data={signals} query={key} isLoading={isLoading} />
        </React.Fragment>
      </Layout>
    );
  }
};
