import React, { useState } from "react";
import type { TableProps } from "@/types";
import { TableBody } from "./TableBody";

export const Table: React.FC<TableProps> = ({ data, query, isLoading }) => {
  // eslint-disable-next-line no-unused-vars, @typescript-eslint/no-unused-vars
  const [rowsPerPage, setRowsPerPage] = useState<number>(25);

  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
        <TableBody
          data={data}
          query={query}
          isLoading={isLoading}
          rowsPerPage={rowsPerPage}
        />
      </div>
    </div>
  );
};
