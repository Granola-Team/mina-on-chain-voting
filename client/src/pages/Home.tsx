import React from "react";
import { useSearchParams } from "react-router-dom";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";
import { Table } from "@/components/Table";

export const Home = () => {
  const [searchParams] = useSearchParams();
  const key = searchParams.get("key");
  const filter = searchParams.get("filter");

  return (
    <Layout>
      <Stats />
      <Table />
      {key && `Search Parameter detected, ${key}`}
      {filter && `Filter Parameter detected, ${filter}`}
    </Layout>
  );
};
