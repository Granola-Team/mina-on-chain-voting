/* eslint-disable no-nested-ternary */
import React from "react";

import type { TableProps } from "@/types";

import { Spinner } from "../Spinner";
import { ResultsTableBodyWrapper } from "./ResultsTableBodyWrapper";
import { ResultsTableRow } from "./ResultsTableRow";

export const ResultsTableBody: React.FC<TableProps> = ({
  data,
  query,
  isLoading,
  stats,
}) => {
  if (isLoading) {
    return (
      <ResultsTableBodyWrapper>
        <div className="py-6 mt-1">
          <Spinner />
        </div>
      </ResultsTableBodyWrapper>
    );
  }

  return (
    <ResultsTableBodyWrapper>
      {data.length > 0 ? (
        data.filter((entity) => entity.signal_status !== "Invalid").map((signal, index) => (
          <ResultsTableRow key={data.length + index} stats={stats} signal={signal} />
        ))
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </ResultsTableBodyWrapper>
  );
};
