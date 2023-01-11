import { useState } from "react";
import type { SignalEntity } from "@/types";

interface UseTableReturnArgs {
  slice: SignalEntity[];
  range: number[];
  next: () => void;
}

// Simplify calculateRange & sliceData = x
// Remove all additions AppStore = check
// Boundaries: Work inside useTable = x

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

export const useTable = (
  data: SignalEntity[],
  rowsPerPage: number,
  ): UseTableReturnArgs => {
  const [page, setPage] = useState(1);
  const [tableRange, setTableRange] = useState<number[]>([]);
  const [slice, setSlice] = useState<SignalEntity[]>([]);

  const next = () => {
    const range = calculateRange(data, rowsPerPage);
    setTableRange([...range]);
    const sliceOfData = sliceData(data, page, rowsPerPage);
    setSlice([...sliceOfData]);
    const currentPage = setPage(React.Dispatch<React.SetStateAction<1>>);
  };

  return { slice, range: tableRange, next };
};

export default useTable;
