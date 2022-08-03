import React from "react";

import type { TableProps } from "@/types";

import { TableHeader } from "./TableHeader";
import { TableRow } from "./TableRow";

//! Comment:
//! Component built for dummy data.

export const TableBody: React.FC<TableProps> = ({ data }) => {
  return (
    <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-md py-2">
      <TableHeader />
      <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed">
        {data[0].signals.map((signal, index) => (
          <TableRow key={signal.timestamp / index} signal={signal} />
        ))}
      </div>
    </div>
  );
};
