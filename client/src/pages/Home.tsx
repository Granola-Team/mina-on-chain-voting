import React, { useState, useEffect } from "react";

import type { DataEntity } from "@/types";

import { useFilterParams } from "@/hooks/useFilterParams";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";
import { Table } from "@/components/Table";

import { unsortedData, settledData, unsettledData, invalidData } from "@/dummy";

//! Comment:
//! Page built for dummy data.

export const Home = () => {
  const [data, setData] = useState<DataEntity[]>();
  const [searchParams] = useFilterParams();
  const key = searchParams.get("key");
  const filter = searchParams.get("filter");

  useEffect(() => {
    switch (filter) {
      case "all":
        setData(unsortedData);
        break;
      case "settled":
        setData(settledData);
        break;
      case "unsettled":
        setData(unsettledData);
        break;
      case "invalid":
        setData(invalidData);
        break;
      default:
        setData(unsortedData);
        break;
    }
  }, [key, filter]);

  return (
    <Layout>
      <Stats />
      {data ? <Table data={data} /> : <div />}
    </Layout>
  );
};
