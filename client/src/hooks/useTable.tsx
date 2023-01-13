import { useEffect, useState } from "react";
import type { SignalEntity } from "@/types";

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

export const useTable = (data: SignalEntity[], rowsPerPage: number) => {
  const [slice, setSlice] = useState<SignalEntity[]>([]);
  const [range, setRange] = useState<number[]>([]);
  const [page, setPage] = useState<number>(1);

  useEffect(() => {
    setRange(calculateRange(data, rowsPerPage));
    setSlice(sliceData(data, page, rowsPerPage));
  }, [data, setRange, page, setSlice, rowsPerPage]);

  return { slice, range, page, setPage };
};

export default useTable;
