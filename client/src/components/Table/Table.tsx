import React from "react";

import type { TableProps } from "@/types";

import { TableNavigation } from "./TableNavigation";
import { TableBody } from "./TableBody";

export const Table: React.FC<TableProps> = ({ data }) => {
  return (
    <div className="w-full">
      <div className="content-full-width">
        <div className="px-8 w-full flex flex-col items-center gap-2">
          <TableNavigation />
          <TableBody data={data} />
        </div>
      </div>
    </div>
  );
};
