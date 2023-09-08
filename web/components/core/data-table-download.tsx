'use client';

import { Button } from 'components/core/button';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
} from 'components/core/dropdown';

import { type Buffer, Workbook } from 'exceljs';
import { saveAs } from 'file-saver';
import moment from 'moment';

import { DropdownMenuTrigger } from '@radix-ui/react-dropdown-menu';
import { DownloadIcon } from '@radix-ui/react-icons';
import { Table } from '@tanstack/react-table';

interface Props<T> {
  table: Table<T>;
}

const formatVariants = [
  {
    name: 'CSV',
    format: 'csv',
  },
  {
    name: 'XLSX',
    format: 'xlsx',
  },
] as const;

const handlers = (format: string, wb: Workbook): (() => Promise<Buffer>) | null => {
  switch (format) {
    case 'csv':
      return async () => await wb.csv.writeBuffer();
    case 'xlsx':
      return async () => await wb.xlsx.writeBuffer();
    default:
      return null;
  }
};

const download = async <T,>(table: Table<T>, format: (typeof formatVariants)[number]['format']) => {
  const formatDetails = formatVariants.find((variant) => variant.format === format);

  if (!formatDetails) return;

  const wb = new Workbook();
  const ws = wb.addWorksheet('Sheet 1');

  const lastHeaderGroup = table.getHeaderGroups().at(-1);
  if (!lastHeaderGroup) {
    console.error('No header groups found', table.getHeaderGroups());
    return;
  }

  ws.columns = lastHeaderGroup.headers.map((header) => {
    return {
      header: header.column.id,
      key: header.id,
      width: 20,
    };
  });

  table.getCoreRowModel().rows.forEach((row) => {
    const values = row.getVisibleCells().map((cell) => cell.getValue() ?? '');
    ws.addRow(values);
  });

  ws.getRow(1).eachCell((cell) => {
    cell.font = { bold: true };
  });

  const handler = handlers(format, wb);

  if (handler) {
    const buffer = await handler();
    const now = moment(new Date()).utc();
    saveAs(new Blob([buffer]), `export-${now.unix()}.${formatDetails.format}`);
  }
};

export const DataTableDownload = <T,>({ table }: Props<T>) => {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" size="sm" className="ml-auto hidden h-8 lg:flex">
          <DownloadIcon className="mr-2 h-4 w-4" />
          <span className="mt-0.5">Download</span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" className="w-[150px]">
        <DropdownMenuLabel>What Format?</DropdownMenuLabel>
        <DropdownMenuSeparator />
        {formatVariants.map((variant) => (
          <DropdownMenuItem key={variant.format} onSelect={async () => await download(table, variant.format)}>
            {variant.name}
          </DropdownMenuItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
