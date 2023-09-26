'use client';

import * as React from 'react';

import { useRouter } from 'next/navigation';

import { DataTablePagination } from 'components/core/data-table-pagination';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from 'components/core/table';

import {
  ColumnDef,
  ColumnFiltersState,
  flexRender,
  getCoreRowModel,
  getFacetedRowModel,
  getFacetedUniqueValues,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  SortingState,
  Table as TTable,
  useReactTable,
  VisibilityState,
} from '@tanstack/react-table';

export const DataTableVariants = ['proposal', 'vote'] as const;
export type DataTableVariant = (typeof DataTableVariants)[number];

interface Props<T, V> {
  data: T[];
  columns: ColumnDef<T, V>[];
  columnVisibility?: VisibilityState;
  Toolbar: React.ComponentType<{ table: TTable<T> }>;
  variant: DataTableVariant;
}

const useSafeRouter = () => {
  try {
    return useRouter();
  } catch (error) {
    return {
      push: () => {},
    };
  }
};

export const DataTable = <T, V>({ columns, columnVisibility, data, Toolbar, variant }: Props<T, V>) => {
  const [columnFilters, setColumnFilters] = React.useState<ColumnFiltersState>([]);
  const [sorting, setSorting] = React.useState<SortingState>([]);
  const router = useSafeRouter();

  const table = useReactTable({
    data,
    columns,
    state: {
      sorting,
      columnFilters,
      columnVisibility,
    },
    enableRowSelection: false,
    onSortingChange: setSorting,
    onColumnFiltersChange: setColumnFilters,
    getCoreRowModel: getCoreRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFacetedRowModel: getFacetedRowModel(),
    getFacetedUniqueValues: getFacetedUniqueValues(),
  });

  return (
    <div className="space-y-4">
      <Toolbar table={table} />
      <div className="rounded-md border">
        <Table>
          <TableHeader>
            {table.getHeaderGroups().map((headerGroup) => (
              <TableRow key={headerGroup.id}>
                {headerGroup.headers.map((header) => {
                  return (
                    <TableHead key={header.id}>
                      {header.isPlaceholder ? null : flexRender(header.column.columnDef.header, header.getContext())}
                    </TableHead>
                  );
                })}
              </TableRow>
            ))}
          </TableHeader>
          <TableBody>
            {table.getRowModel().rows?.length ? (
              table.getRowModel().rows.map((row) => (
                <TableRow
                  key={row.id}
                  className={variant === 'proposal' ? 'cursor-pointer' : undefined}
                  onClick={
                    variant === 'proposal'
                      ? () => {
                          const proposalId = row.getValue('id');
                          router.push(
                            row.getValue('status') === 'Completed'
                              ? `/proposal/${proposalId}/results`
                              : `/proposal/${proposalId}`
                          );
                        }
                      : undefined
                  }
                >
                  {row.getVisibleCells().map((cell) => (
                    <TableCell key={cell.id}>{flexRender(cell.column.columnDef.cell, cell.getContext())}</TableCell>
                  ))}
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell colSpan={columns.length} className="h-24 text-center">
                  No results.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
      <DataTablePagination table={table} />
    </div>
  );
};
