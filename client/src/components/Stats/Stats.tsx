import React from "react";

export const Stats = () => {
  return (
    <div className="flex items-center justify-center border-b bg-grayA-2 border-gray-7">
      <div className="flex items-center justify-center w-1/2">
        <div className="flex items-center w-full gap-6 py-6">
          <StatsBubble />
          <StatsBubble />
          <StatsBubble />
        </div>
      </div>
    </div>
  );
};

export const StatsBubble = () => {
  return (
    <div className="w-full py-8 border rounded-xl border-gray-11">
      <div></div>
    </div>
  );
};
