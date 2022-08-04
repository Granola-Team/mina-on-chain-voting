import React, { useState, useEffect } from "react";
import { useQuery } from "@tanstack/react-query";

import type { DataEntity } from "@/types";

import { useFilterParams } from "@/hooks/useFilterParams";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";
import { Table } from "@/components/Table";

import { fetchKeywordData } from "./Home.queries";

import { unsortedData, settledData, unsettledData, invalidData } from "@/dummy";

export const Home = () => {
  const [data, setData] = useState<DataEntity[]>();
  const [searchParams] = useFilterParams();
  const key = searchParams.get("key");
  const filter = searchParams.get("filter");

  // Param to toggle demonstration mode.
  const demo = searchParams.get("demo");

  const { data: queryData, isSuccess } = useQuery(
    [key, filter],
    () => fetchKeywordData(key!, filter!),
    { enabled: !demo },
  );

  useEffect(() => {
    if (isSuccess) {
      setData(queryData);
    }
  }, [queryData, isSuccess]);

  useEffect(() => {
    if (demo === "true") {
      switch (filter) {
        case "All":
          setData(unsortedData);
          break;
        case "Settled":
          setData(settledData);
          break;
        case "Unsettled":
          setData(unsettledData);
          break;
        case "Invalid":
          setData(invalidData);
          break;
        default:
          setData(unsortedData);
          break;
      }
    }
  }, [demo, key, filter]);

  return (
    <Layout>
      <Stats />
      {data ? <Table data={data} /> : <div />}
    </Layout>
  );
};
