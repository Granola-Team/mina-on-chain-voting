import React from "react";

import type { TableBubbleProps } from "@/types";

export const TableBubble: React.FC<TableBubbleProps> = ({
  children,
  status,
}) => {
  const getColor = () => {
    if (status === "Canonical" || status === "Settled") {
      return "bg-greenA-4 border-greenA-7";
    }
    if (status === "Pending" || status === "Unsettled") {
      return "bg-yellowA-4 border-yellowA-7";
    }
    if (status === "Orphaned" || status === "Invalid") {
      return "bg-redA-4 border-redA-7";
    }

    return "";
  };

  return (
    <div
      className={`flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 ${getColor()}`}
    >
      {children}
    </div>
  );
};
