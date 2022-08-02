import React from "react";

export const Stats = () => {
  return (
    <div className="border-b bg-grayA-2 border-gray-7">
      <div className="py-3 content-full-width">
        <div className="flex items-center gap-6 px-8 w-full">
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
    <div className="w-full py-8 rounded-xl bg-gray-2 border border-gray-5">
      <div></div>
    </div>
  );
};
