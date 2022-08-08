import React from "react";

import { StatsRowElement } from "./StatsRowElement";

export const StatsRow = () => {
  return (
    <div className="border-b bg-gray-2 border-gray-7">
      <div className="py-3 content-full-width">
        <div className="flex items-center gap-6 px-8 w-full">
          <StatsRowElement />
          <StatsRowElement />
          <StatsRowElement />
        </div>
      </div>
    </div>
  );
};
