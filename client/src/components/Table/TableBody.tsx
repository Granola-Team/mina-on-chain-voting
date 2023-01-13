import React from "react";
import type { SliceTableProps } from "@/types";
import { Spinner } from "../Spinner";
import { TableBodyWrapper } from "./TableBodyWrapper";
import { TableRow } from "./TableRow";
import { TableFooter } from "./TableFooter";
import { useTable } from "@/hooks/useTable";

export const TableBody: React.FC<SliceTableProps> = ({
  data,
  query,
  isLoading,
  rowsPerPage,
}) => {
  const { slice, range, page, setPage } = useTable(data, rowsPerPage);

  if (isLoading) {
    return (
      <TableBodyWrapper>
        <div className="py-6 mt-1">
          <Spinner />
        </div>
      </TableBodyWrapper>
    );
  }

  return (
    <TableBodyWrapper>
      {slice.length > 0 ? (
        <>
          {slice.map((signal, index) => (
            <TableRow key={slice.length + index} signal={signal} />
          ))}
          <TableFooter
            range={range}
            setPage={setPage}
            page={page}
            slice={slice}
          />
        </>
      ) : (
        <span className="text-md py-12 medium">
          No results found for keyword &apos;{query ? query : "_"}&apos;
        </span>
      )}
    </TableBodyWrapper>
  );
};
