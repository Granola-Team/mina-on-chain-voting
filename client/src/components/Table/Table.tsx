import React from "react";
import type { TableProps } from "@/types";
import { TableBody } from "./TableBody";
import { useTable } from "@/hooks/usePagination";

export const Table: React.FC<TableProps> = ({ data, query, isLoading }) => {
  const { slice, range, next } = useTable(data, 25);

  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
      <TableBody data={slice} query={query} isLoading={isLoading} />
      </div>
    </div>
  );
};
