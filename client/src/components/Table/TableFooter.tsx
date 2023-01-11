import React, { useEffect } from "react";
import styles from "./TableFooter.module.css";
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
    <div className={styles.tableFooter}>
      {range.map((el: number, index: number) => (
        <button
          type="button"
          key={index}
          className={`${styles.button} ${
            page === el ? styles.activeButton : styles.inactiveButton
          }`}
          onClick={() => {
            setPage(el);
          }}
        >
          {el}
        </button>
      ))}
    </div>
  );
};
