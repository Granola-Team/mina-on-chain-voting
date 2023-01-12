/* eslint-disable no-nested-ternary */
import React, { useEffect } from "react";

import type { TableProps } from "@/types";

import { Spinner } from "../Spinner";
import { TableBodyWrapper } from "./TableBodyWrapper";
import { TableRow } from "./TableRow";

import { TableFooter } from "./TableFooter";
import { useTable } from "@/hooks/usePagination";

export const TableBody: React.FC<TableProps> = ({ data, query, isLoading }) => {
  const { slice, range, next } = useTable(data, 25);

  useEffect(() => {
    console.log(slice);
  },[slice])

  if (isLoading) {
    return (
      <TableBodyWrapper>
        <div className="py-6 mt-1">
          <Spinner />
        </div>
      </TableBodyWrapper>
    );
  }

  return (
    <TableBodyWrapper>
      {data.length > 0 ? (
        <>
          {data.map((signal, index) => (
            <TableRow key={data.length + index} signal={signal} />
          ))}
          <TableFooter range={range} setPage={next} page={1} slice={slice} />
        </>
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </TableBodyWrapper>
  );
};
