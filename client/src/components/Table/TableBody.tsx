import React from "react";

import type { TableProps } from "@/types";

import { useAppStore } from "@/store/app.store";
import { useFilterParams } from "@/hooks/useFilterParams";

import { TableHeader } from "./TableHeader";
import { TableRow } from "./TableRow";

export const TableBody: React.FC<TableProps> = ({ data }) => {
  const isFetching = useAppStore((state) => state.isFetching);
  const [searchParams] = useFilterParams();
  const key = searchParams.get("key");

  if (isFetching) {
    return (
      <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-md py-2">
        <TableHeader />
        <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed">
          <span>Loading...</span>
        </div>
      </div>
    );
  }

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
