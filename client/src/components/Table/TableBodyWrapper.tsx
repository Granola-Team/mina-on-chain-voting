import React from "react";

import type { ComponentWithChildren } from "@/types";
import { TableHeader } from "./TableHeader";

export const TableBodyWrapper: React.FC<ComponentWithChildren> = ({
  children,
}) => {
  return (
    <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-b-xl py-1 md:py-2">
      <TableHeader />
      <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed">
        {children}
      </div>
    </div>
  );
};
