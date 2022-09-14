import React from "react";
import { useMediaQuery } from "@react-hook/media-query";

export const TableHeader = () => {
  const isMobile = useMediaQuery("only screen and (max-width: 768px)");

  if (isMobile) {
    return null;
  }

  return (
    <div className="grid-table-el w-full mt-[0.1rem] pb-1 border-b border-gray-7">
      <span className="grid-table-heading place-self-center">Blockheight</span>
      <span className="hidden lg:block grid-table-heading place-self-center">
        Timestamp
      </span>
      <span className="grid-table-heading place-self-center lg:place-self-left">
        Account
      </span>
      <span className="grid-table-heading place-self-center">
        Delegated Stake
      </span>
      <span className="grid-table-heading place-self-center">Total %</span>
      <span className="grid-table-heading place-self-center">Delegators</span>
      <span className="grid-table-heading place-self-center">Memo</span>
      <span className="grid-table-heading place-self-center">Blockstatus</span>
      <span className="grid-table-heading place-self-center">
        Classification
      </span>
    </div>
  );
};
