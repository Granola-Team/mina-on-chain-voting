import React, { FC } from "react";

import { TableRowProps } from "@/types";

export const TableRow: FC<TableRowProps> = ({ signal }) => {
  return (
    <div className="grid-table-el w-full py-3">
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
        <span className="grid-table-content">{signal.block_status}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">{signal.signal_status}</span>
      </div>
    </div>
  );
};
