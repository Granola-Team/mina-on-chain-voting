import React from "react";

import type { TableProps } from "@/types";

import { ResultsTableBody } from "./ResultsTableBody";

export const ResultsTable: React.FC<TableProps> = ({
  data,
  query,
  isLoading,
}) => {
  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
        <ResultsTableBody data={data} query={query} isLoading={isLoading} />
      </div>
    </div>
  );
};
