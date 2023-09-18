import * as React from 'react';

import { cleanup, render, screen, TestVariant } from 'common/test';

import { ProposalTable } from 'components/proposal-table';

import { mockDeep } from 'jest-mock-extended';
import moment from 'moment';

type Proposal = React.ComponentProps<typeof ProposalTable>['proposals'][number];

describe(TestVariant.Component, () => {
  describe(ProposalTable, () => {
    let user: ReturnType<typeof render>['user'];

    const proposals = [
      mockDeep<Proposal>({
        id: 1,
        key: 'MIP1',
        category: 'Core',
        title: 'Remove supercharged rewards',
        status: 'Pending',
        start_time: 1698253600000,
        end_time: 1798253600000,
      }),
      mockDeep<Proposal>({
        id: 2,
        key: 'MIP2',
        category: 'Cryptography',
        title: 'Kimchi, a new proof system',
        status: 'In Progress',
        start_time: 1645253600000,
        end_time: 1698753600000,
      }),
      mockDeep<Proposal>({
        id: 3,
        key: 'MIP3',
        category: 'ERC',
        title: 'Easier zkApp programmability on mainnet',
        status: 'In Review',
        start_time: 1684562400000,
        end_time: 1685253600000,
      }),
      mockDeep<Proposal>({
        id: 4,
        key: 'MIP4',
        category: 'Networking',
        title: 'Add Prometheus log monitoring',
        status: 'Completed',
        start_time: 1672848000000,
        end_time: 1673685000000,
      }),
      mockDeep<Proposal>({
        id: 5,
        key: 'MIP5',
        category: 'Interface',
        title: 'Renders invalid status',
        status: 'Unknown',
        start_time: 1671948000000,
        end_time: 1674689000000,
      }),
    ];

    beforeEach(() => {
      const result = render(<ProposalTable proposals={proposals} />);
      user = result.user;
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.getByRole('button', { name: /Status/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Category/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Download/i })).toBeVisible();

      proposals.forEach((item) => {
        expect(screen.getByText(`${item.key}`)).toBeVisible();
        expect(screen.getByText(item.category)).toBeVisible();
        expect(screen.getByText(item.title)).toBeVisible();
        expect(screen.getByText(item.status)).toBeVisible();

        const format = 'YYYY-MM-DD - hh:mm';
        const startDate = moment(new Date(item.start_time)).utc();
        const endDate = moment(new Date(item.end_time)).utc();

        expect(screen.getByText(startDate.format(format).toString())).toBeVisible();
        expect(screen.getByText(endDate.format(format).toString())).toBeVisible();
      });
    });

    it('filters proposals by category', async () => {
      const categoryButton = screen.getByRole('button', { name: /Category/i });
      await user.click(categoryButton);

      expect(screen.getByRole('listbox')).toBeVisible();

      proposals.forEach(async (item) => {
        const optionName = new RegExp(item.category, 'i');
        const option = screen.getByRole('option', { name: optionName });

        expect(option).toBeVisible();
        await user.click(option);
        expect(screen.getByText(item.category)).toBeVisible();

        proposals.forEach((other) => {
          if (other.id === item.id) return;
          expect(screen.queryByText(other.title)).not.toBeInTheDocument();
        });

        const resetButton = screen.getByRole('button', { name: /Reset/i });
        expect(resetButton).toBeVisible();
        await user.click(resetButton);
      });
    });
  });
});
