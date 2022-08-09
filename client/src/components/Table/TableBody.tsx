import React from "react";

import type { TableProps } from "@/types";

import { useFilterParams } from "@/hooks/useFilterParams";

import { TableHeader } from "./TableHeader";
import { TableRow } from "./TableRow";

export const TableBody: React.FC<TableProps> = ({ data }) => {
  const [searchParams] = useFilterParams();
  const key = searchParams.get("key");

  return (
    <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-md py-2">
      <TableHeader />
      <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed">
        {data.signals.length > 0 ? (
          data.signals.map((signal, index) => (
            <TableRow key={signal.timestamp / index} signal={signal} />
          ))
        ) : (
          <span className="text-md py-12 medium">
            No results found for &apos;{key}&apos;
          </span>
        )}
      </div>
    </div>
  );
};
