import React, { useEffect } from "react";

import styles from "./TableFooter.module.css";
import { useTable } from "../../hooks/useTable";
import { useAppStore } from "@/App.store";
import type { SignalEntity } from "@/types";

// needs type conversions from JS to TS

const TableFooter = ({ range, setPage, page, slice }) => {
  useEffect(() => {
    if (slice.length < 1 && page !== 1) {
      setPage(page - 1);
    }
  }, [slice, page, setPage]);
  return (
    <div className={styles.tableFooter}>
      {range.map((el: number, index: number) => (
        <button
          key={index}
          className={`${styles.button} ${
            page === el ? styles.activeButton : styles.inactiveButton
          }`}
          onClick={() => setPage(el)}
        >
          {el}
        </button>
      ))}
    </div>
  );
};
