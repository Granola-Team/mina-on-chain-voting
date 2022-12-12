import React from "react";

import type { TableProps } from "@/types";

import { TableBody } from "./TableBody";

export const Table: React.FC<TableProps> = ({
  data,
  query,
  isLoading,
  stats,
}) => {
  return (
    <div className="content-full-width">
      <div className="px-2 md:px-4 lg:px-8 w-full flex flex-col items-center">
        <TableBody
          data={data}
          stats={stats}
          query={query}
          isLoading={isLoading}
        />
      </div>
    </div>
  );
};
