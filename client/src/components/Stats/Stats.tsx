import React from "react";

export const StatsElement = () => {
  return (
    <div className="flex items-center justify-center w-full py-4 rounded-xl bg-gray-3 border border-gray-7">
      <span>Statistics</span>
    </div>
  );
};

export const Stats = () => {
  return (
    <div className="border-b bg-gray-2 border-gray-7">
      <div className="py-3 content-full-width">
        <div className="flex items-center gap-6 px-8 w-full">
          <StatsElement />
          <StatsElement />
          <StatsElement />
        </div>
      </div>
    </div>
  );
};
