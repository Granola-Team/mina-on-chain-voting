import { useEffect, useState } from "react";
import type { SignalEntity } from "@/types";

/*
interface UseTableReturnArgs {
  slice: SignalEntity[];
  range: number[];
  rowsPerPage: number;
}
*/
// next: () => void;

const calculateRange = (data: SignalEntity[], rowsPerPage: number) => {
  const range: number[] = [];
  const num = Math.ceil(data.length / rowsPerPage);
  for (let i = 1; i <= num; i++) {
    range.push(i);
  }
  return range;
};

const sliceData = (data: SignalEntity[], page: number, rowsPerPage: number) => {
  return data.slice((page - 1) * rowsPerPage, page * rowsPerPage);
};

export const useTable = (
  data: SignalEntity[],
  page: number,
  rowsPerPage: number,
  ) => {
  const [tableRange, setTableRange] = useState<number[]>([]); // need default value for initial slice
  const [slice, setSlice] = useState<SignalEntity[]>([]); // need default value for initial slice

  useEffect(() => {
    const range = calculateRange(data, rowsPerPage);
    setTableRange([...range]);
    const slice = sliceData(data, page, rowsPerPage);
    setSlice([...slice]); // sliceOfData or something else
  }, [data, setTableRange, page, setSlice, rowsPerPage]);

  return { slice, range: tableRange };
};

export default useTable;
