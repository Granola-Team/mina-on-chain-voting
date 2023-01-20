/* eslint-disable no-nested-ternary */
import React from "react";

import type { TableProps } from "@/types";

import { Spinner } from "../Spinner";
import { ResultsTableBodyWrapper } from "./ResultsTableBodyWrapper";
import { ResultsTableRow } from "./ResultsTableRow";
import useTable from "@/hooks/useTable";
import { TableFooter } from "../Table/TableFooter";

export const ResultsTableBody: React.FC<TableProps> = ({
  data,
  query,
  isLoading,
}) => {
  const { slice, range, page, setPage } = useTable(data, 20);

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
        <>
          {slice.map((signal, index) => (
            <ResultsTableRow key={slice.length + index} signal={signal} />
          ))}
          <TableFooter
            range={range}
            setPage={setPage}
            page={page}
            slice={slice}
          />
        </>
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </ResultsTableBodyWrapper>
  );
};
