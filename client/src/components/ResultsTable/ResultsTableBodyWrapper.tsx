import React from "react";

import type { ComponentWithChildren } from "@/types";
import { ResultsTableHeader } from "./ResultsTableHeader";

export const ResultsTableBodyWrapper: React.FC<ComponentWithChildren> = ({
  children,
}) => {
  return (
    <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-b-xl py-1 md:py-2">
      <ResultsTableHeader />
      <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed">
        {children}
      </div>
    </div>
  );
};
