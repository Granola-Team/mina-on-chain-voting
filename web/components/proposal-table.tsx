'use client';

import type { ForwardRefExoticComponent, RefAttributes } from 'react';

import Link from 'next/link';

import { GetProposalListResult } from 'common/store';

import { Badge } from 'components/core/badge';
import { Button } from 'components/core/button';
import { DataTable } from 'components/core/data-table';
import { DataTableColumnHeader } from 'components/core/data-table-column-header';
import { HoverCard, HoverCardContent, HoverCardTrigger } from 'components/core/hover-card';
import { ProposalTableToolbar } from 'components/proposal-table-toolbar';

import { ProposalCategory, ProposalListParserOutcome, ProposalStatus } from 'models';
import moment from 'moment';

import {
  CheckCircledIcon,
  CountdownTimerIcon,
  ExternalLinkIcon,
  QuestionMarkCircledIcon,
  StopwatchIcon,
} from '@radix-ui/react-icons';
import type { IconProps } from '@radix-ui/react-icons/dist/types';
import { ColumnDef } from '@tanstack/react-table';

interface Props {
  proposals: GetProposalListResult;
}

interface ProposalTableStatus {
  value: ProposalStatus;
  icon: ForwardRefExoticComponent<IconProps & RefAttributes<SVGSVGElement>>;
  description: string;
}

interface ProposalTableCategory {
  value: ProposalCategory;
}

export const proposalTableStatuses = [
  {
    value: 'Pending',
    icon: CountdownTimerIcon,
    description: 'The voting period has not begun.',
  },
  {
    value: 'In Progress',
    icon: StopwatchIcon,
    description: 'Voting is in progress.',
  },
  {
    value: 'In Review',
    icon: QuestionMarkCircledIcon,
    description: 'Voting has closed and the results are being verified.',
  },
  {
    value: 'Completed',
    icon: CheckCircledIcon,
    description: 'Voting has finished and the results have been verified.',
  },
] satisfies ProposalTableStatus[];

export const proposalTableCategories = [
  {
    value: 'Core',
  },
  {
    value: 'Networking',
  },
  {
    value: 'Interface',
  },
  {
    value: 'ERC',
  },
  {
    value: 'Cryptography',
  },
] satisfies ProposalTableCategory[];

const columns: ColumnDef<ProposalListParserOutcome[number]>[] = [
  {
    accessorKey: 'id',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Key" className="pl-2.5" />,
    cell: ({ row }) => (
      <div className="flex pl-2.5">
        <span className="whitespace-nowrap">{`${row.original.key}`}</span>
      </div>
    ),
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'category',
    filterFn: (row, _id, value) => {
      return value.includes(row.original.category);
    },
  },
  {
    accessorKey: 'title',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Title" />,
    cell: ({ row }) => {
      return (
        <div className="flex space-x-2 min-w-[350px]">
          <Badge variant="outline" className="bg-gray-200/10 dark:bg-gray-500/10">
            {row.original.category}
          </Badge>
          <span className="truncate font-medium">{row.getValue('title')}</span>
        </div>
      );
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'status',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Status" />,
    cell: ({ row }) => {
      const status = proposalTableStatuses.find((status) => status.value === row.getValue('status')) ?? {
        value: 'Unknown',
        icon: QuestionMarkCircledIcon,
        description: 'The status of this proposal is unknown.',
      };

      return (
        <HoverCard openDelay={200} closeDelay={100}>
          <HoverCardTrigger>
            <div className="flex w-[100px] items-center gap-1.5">
              <status.icon className="h-4 w-4 text-muted-foreground" />
              <span className="select-none">{status.value}</span>
            </div>
          </HoverCardTrigger>
          <HoverCardContent>
            <div className="flex">
              <span className="text-xs select-none">{status.description}</span>
            </div>
          </HoverCardContent>
        </HoverCard>
      );
    },
    filterFn: (row, id, value) => {
      return value.includes(row.getValue(id));
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'start_time',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Voting Start" />,
    cell: ({ row }) => {
      const startDate = moment(new Date(row.getValue('start_time'))).utc();
      return <span className="whitespace-nowrap">{startDate.format('YYYY-MM-DD - hh:mm').toString()}</span>;
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'end_time',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Voting End" />,
    cell: ({ row }) => {
      const endDate = moment(new Date(row.getValue('end_time'))).utc();
      return <span className="whitespace-nowrap">{endDate.format('YYYY-MM-DD - hh:mm').toString()}</span>;
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    id: 'actions',
    cell: ({ row }) => {
      const buttonText = row.getValue('status') === 'Completed' ? 'Results' : 'Go Vote';

      return (
        <Link
          href={
            row.getValue('status') === 'Completed'
              ? `/proposal/${row.getValue('id')}/results`
              : `/proposal/${row.getValue('id')}`
          }
        >
          <Button variant="ghost" className="flex h-8 w-auto p-0 data-[state=open]:bg-muted">
            <div className="flex items-center">
              <span className="ml-1 whitespace-nowrap">{buttonText}</span>
              <ExternalLinkIcon className="h-4 w-4" />
            </div>
            <span className="sr-only">Open menu</span>
          </Button>
        </Link>
      );
    },
  },
];

export const ProposalTable = ({ proposals }: Props) => {
  return (
    <DataTable
      data={proposals}
      columns={columns}
      columnVisibility={{ category: false }}
      Toolbar={ProposalTableToolbar}
      variant="proposal"
    />
  );
};
