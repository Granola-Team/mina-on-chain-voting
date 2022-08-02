import React, { useState, useEffect } from "react";
import { useSearchParams } from "react-router-dom";

import type { SignalEntity } from "@/types";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";
import { Table } from "@/components/Table";

import { dummyData } from "@/dummy";

export const Home = () => {
  const [data, setData] = useState<SignalEntity[]>(dummyData[0].results);
  const [searchParams] = useSearchParams();
  const key = searchParams.get("key");
  const filter = searchParams.get("filter");

  useEffect(() => {
    // Parse Data
  }, [key, filter]);

  return (
    <Layout>
      <Stats />
      <Table />
      {key && `Search Parameter detected, ${key}`}
      {filter && `Filter Parameter detected, ${filter}`}
    </Layout>
  );
};
