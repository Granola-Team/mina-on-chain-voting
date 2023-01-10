import { useState, useEffect } from "react";

import { useAppStore } from "@/App.store";
import type { SignalEntity } from "@/types";
import shallow from "zustand/shallow";

const calculateRange = (data: SignalEntity[], rowsPerPage: number) => {
  const range: number[] = [];
  const num = Math.ceil(data.length / rowsPerPage);
  for (let i = 1; i <= num; i++) {
    range.push(i);
  }
  return range;
};

const sliceData = (data: SignalEntity[], page: number, rowsPerPage: number) => {
  const sliceOfData = data.slice((page - 1) * rowsPerPage, page * rowsPerPage);
  return sliceOfData;
};

const useTable = (data: SignalEntity[], page: number, rowsPerPage: number) => {
  const [tableRange, setTableRange] = useState([]);
  const { slice, setSlice } = useAppStore(
    (state) => ({ slice: state.slice, setSlice: state.setSlice }),
    shallow,
  );

  useEffect(() => {
    const range = calculateRange(data, rowsPerPage);
    setTableRange([...range]);

    const sliceOfData = sliceData(data, page, rowsPerPage);
    setSlice([...sliceOfData]);
  }, [data, setTableRange, page, setSlice, rowsPerPage]); // may be without rowsPerPage

  return { slice, range: tableRange };
};

export default useTable;
