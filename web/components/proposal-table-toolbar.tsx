'use client';

import { Button } from 'components/core/button';
import { DataTableDownload } from 'components/core/data-table-download';
import { DataTableFilter } from 'components/core/data-table-filter';
import { Input } from 'components/core/input';
import { proposalTableCategories, proposalTableStatuses } from 'components/proposal-table';

import { Cross2Icon } from '@radix-ui/react-icons';
import { Table } from '@tanstack/react-table';

interface Props<T> {
  table: Table<T>;
}

export const ProposalTableToolbar = <T,>({ table }: Props<T>) => {
  const isFiltered = table.getState().columnFilters.length > 0;

  return (
    <div className="flex items-center justify-between">
      <div className="flex flex-1 items-center space-x-2">
        <Input
          placeholder="Find proposal..."
          value={(table.getColumn('title')?.getFilterValue() as string) ?? ''}
          onChange={(event) => table.getColumn('title')?.setFilterValue(event.target.value)}
          className="h-8 w-[150px] lg:w-[250px]"
        />

        <DataTableFilter column={table.getColumn('status')} title="Status" options={proposalTableStatuses} />
        <DataTableFilter column={table.getColumn('category')} title="Category" options={proposalTableCategories} />

        {isFiltered && (
          <Button variant="ghost" onClick={() => table.resetColumnFilters()} className="h-8 px-2 lg:px-3">
            Reset
            <Cross2Icon className="ml-2 h-4 w-4" />
          </Button>
        )}
      </div>

      <DataTableDownload table={table} />
    </div>
  );
};
