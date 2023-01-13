import React, { useEffect } from "react";
import type { SignalEntity } from "@/types";

interface TableFooterProps {
  range: number[];
  setPage: React.Dispatch<React.SetStateAction<number>>;
  page: number;
  slice: SignalEntity[];
}

export const TableFooter = ({ range, setPage, page, slice }: TableFooterProps) => {
  useEffect(() => {
    if (slice.length < 1 && page !== 1) {
      setPage(page - 1);
    }
  }, [slice, page, setPage]);

  return (
    <div className="border-solid">
    <div className="place-self-center space-x-10">
      {range.map((el: number, index: number) => (
        <button
          type="button"
          key={index}
          className={`cursor-pointer space-x-4 ${
            page === el ? `text-blue-400` : `grid-table-content-mobile semibold`
          }`}
          onClick={() => {
            setPage(el);
          }}
        >
          {el}
        </button>
      ))}
    </div>
    </div>
  );
};
