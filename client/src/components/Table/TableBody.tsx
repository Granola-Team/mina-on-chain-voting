import React from "react";

import { TableHeader } from "./TableHeader";
import { TableRow } from "./TableRow";

export const TableBody = () => {
  return (
    <div className="w-full flex flex-col bg-gray-2 border border-gray-7 rounded-md py-2">
      <TableHeader />
      <div className="flex items-center flex-col divide-y divide-gray-7 divide-dashed mt-2">
        <TableRow
          signal={{
            timestamp: 1607110465663,
            account: "B62qq76YZRp4QNjd7r4qmrNWAtLF2FbR3vnad8SMsBgtvJUHw7TPPnZ",
            memo: "Magenta",
            block_status: "Canonical",
            signal_status: "Settled",
            height: 92903,
          }}
        />
      </div>
    </div>
  );
};
