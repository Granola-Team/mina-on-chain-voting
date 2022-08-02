import React from "react";

import type { TableRowProps } from "@/types";

export const TableRow: React.FC<TableRowProps> = ({ signal }) => {
  return (
    <div className="grid-table-el w-full py-3 hover:bg-grayA-3 transition-colors duration-100">
      <div className="place-self-center">
        <span className="grid-table-content semibold">{signal.height}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content medium">
          {new Date(signal.timestamp).toLocaleDateString()}
        </span>
      </div>
      <div className="place-self-left">
        <span className="grid-table-content">{signal.account}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">{signal.memo}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">{signal.status}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">{signal.signal_status}</span>
      </div>
    </div>
  );
};
