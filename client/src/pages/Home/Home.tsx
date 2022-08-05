import { useState, useEffect } from "react";
import { useQuery } from "@tanstack/react-query";

import type { DataEntity } from "@/types";

import { useFilterParams } from "@/hooks/useFilterParams";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";
import { Table } from "@/components/Table";

import { fetchKeywordData } from "./Home.queries";

import { unsortedData, settledData, unsettledData, invalidData } from "@/dummy";

export const Home = () => {
  /**
   * @param {DataEntity[]} signals - Local state.
   * @param {() => DataEntity[]} setSignals - Local state setter.
   */
  const [signals, setSignals] = useState<DataEntity[]>();

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
  const { data: queryData, isSuccess } = useQuery(
    [key, filter ? filter : "All"],
    () => fetchKeywordData(key, filter),
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

  return (
    <Layout>
      <Stats />
      {(signals && key) || (signals && demo) ? (
        <Table data={signals} />
      ) : (
        <div className="mx-auto">Please input a keyword.</div>
      )}
    </Layout>
  );
};