import React from "react";

import type { BlockStatus, TableBubbleProps } from "@/types";

const getColor = (status: BlockStatus): string => {
  if (status === "Canonical") {
    return "bg-greenA-4 border-greenA-7";
  }
  if (status === "Pending") {
    return "bg-yellowA-4 border-yellowA-7";
  }
  if (status === "Orphaned") {
    return "bg-redA-4 border-redA-7";
  }

  return "";
};

export const ResultsTableBubble: React.FC<TableBubbleProps> = ({
  children,
  status,
}) => {
  return (
    <div
      className={`flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 ${getColor(
        status,
      )}`}
    >
      {children}
    </div>
  );
};
