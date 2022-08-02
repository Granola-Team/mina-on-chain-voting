import React from "react";

import { TableHeader } from "./TableHeader";
import { TableBody } from "./TableBody";

export const Table = () => {
  return (
    <div className="w-full">
      <div className="content-full-width py-4">
        <div className="px-8 w-full flex flex-col items-center gap-6">
          <TableHeader />
          <TableBody />
        </div>
      </div>
    </div>
  );
};
