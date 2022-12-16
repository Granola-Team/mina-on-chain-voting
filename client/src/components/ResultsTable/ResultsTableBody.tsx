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
        data.map((signal, index) => (
          <ResultsTableRow key={data.length + index} signal={signal} />
        ))
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </ResultsTableBodyWrapper>
  );
};
