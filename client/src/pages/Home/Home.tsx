import React from "react";
import { useNavigate } from "react-router-dom";

import { Layout } from "@/components/Layout";

export const Home = () => {
  const navigate = useNavigate();
  return (
    <Layout>
      <div className="mx-auto pt-10 flex flex-col items-center gap-4">
        <span>Please enter a keyword.</span>
        <button
          onClick={() => {
            navigate("/magenta?demo=true");
          }}
          type="button"
          className="bg-gray-6 rounded-lg px-4 py-1 text-sm semibold hover:bg-gray-7"
        >
          Demo Mode
        </button>
      </div>
    </Layout>
  );
};
