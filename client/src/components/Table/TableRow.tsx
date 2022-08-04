import React from "react";
import makeBlockie from "ethereum-blockies-base64";

import type { TableRowProps } from "@/types";

import { TableBubble } from "./TableBubble";

//! Comment:
//! Component built for dummy data.

export const TableRow: React.FC<TableRowProps> = ({ signal }) => {
  return (
    <div className="grid-table-el w-full py-2 hover:bg-grayA-3 transition-colors duration-100">
      <div className="place-self-center">
        <span className="grid-table-content semibold">{signal.height}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content medium">
          {new Date(signal.timestamp).toLocaleDateString()}
        </span>
      </div>
      <div className="place-self-left flex items-center gap-3">
        <img
          className="w-6 h-6 rounded-full opacity-70"
          src={makeBlockie(signal.account)}
        />
        <span className="grid-table-content medium">{signal.account}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content medium">{signal.memo}</span>
      </div>
      <div className="place-self-center">
        <TableBubble status={signal.status}>
          <span className="grid-table-content medium">{signal.status}</span>
        </TableBubble>
      </div>
      <div className="place-self-center">
        <TableBubble status={signal.signal_status}>
          <span className="grid-table-content medium">
            {signal.signal_status}
          </span>
        </TableBubble>
      </div>
    </div>
  );
};
