/* eslint-disable no-nested-ternary */
import React from "react";

import type { TableProps } from "@/types";

import { Spinner } from "../Spinner";
import { TableBodyWrapper } from "./TableBodyWrapper";
import { TableRow } from "./TableRow";

import { TableFooter } from "./TableFooter";
import { useTable } from "@/hooks/usePagination";

export const TableBody: React.FC<TableProps> = ({ data, query, isLoading }) => {
  const { slice, range, next } = useTable(data, 25);

  if (isLoading) {
    return (
      <TableBodyWrapper>
        <TableFooter range={range} setPage={next} page={1} slice={slice} />
        <div className="py-6 mt-1">
          <Spinner />
        </div>
      </TableBodyWrapper>
    );
  }

  return (
    <TableBodyWrapper>
      {data.length > 0 ? (
        data.map((signal, index) => (
          <TableRow key={data.length + index} signal={signal} />
        ))
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </TableBodyWrapper>
  );
};
