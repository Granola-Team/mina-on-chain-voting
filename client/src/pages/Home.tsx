import React from "react";
import { useSearchParams } from "react-router-dom";

import { Layout } from "@/components/Layout";
import { Stats } from "@/components/Stats";

export const Home = () => {
  const [searchParams] = useSearchParams();
  const key = searchParams.get("key");

  return (
    <Layout>
      <Stats />
      {key && `Search Parameter detected, ${key}`}
    </Layout>
  );
};
