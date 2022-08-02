import React from "react";

export const TableHeader = () => {
  return (
    <div className="grid-table-el w-full mt-[0.1rem] pb-1 border-b border-gray-7">
      <span className="grid-table-heading place-self-center">Blockheight</span>
      <span className="grid-table-heading place-self-center">Timestamp</span>
      <span className="grid-table-heading place-self-left">Account</span>
      <span className="grid-table-heading place-self-center">Memo</span>
      <span className="grid-table-heading place-self-center">Blockstatus</span>
      <span className="grid-table-heading place-self-center">
        Classification
      </span>
    </div>
  );
};
