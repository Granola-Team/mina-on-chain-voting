import React from "react";
import type { TableProps, SliceTableProps } from "@/types";
import { TableBody } from "./TableBody";

export const Table: React.FC<SliceTableProps> = ({ data, query, isLoading, rowsPerPage }) => {
  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
      <TableBody data={data} query={query} isLoading={isLoading} rowsPerPage={rowsPerPage} />
      </div>
    </div>
  );
};
