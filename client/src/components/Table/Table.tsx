import React, { useState } from "react";

import type { TableProps } from "@/types";

import { TableBody } from "./TableBody";
import { TableFooter} from "./TableFooter";
import { useTable } from "../../hooks/useTable";

// rowsPerPage needs to be a prop of TableProps???

export const Table: React.FC<TableProps> = ({ data, query, isLoading, rowsPerPage }) => {
  const [page, setPage] = useState(1);
  const { slice, range } = useTable(data, page, rowsPerPage);
  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
        <TableBody data={data} query={query} isLoading={isLoading} rowsPerPage={25} />
        <TableFooter range={range} slice={slice} setPage={setPage} page={page} />
      </div>
    </div>
  );
};
