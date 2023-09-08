import * as React from 'react';

import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesTable } from 'components/votes-table';

import { mockDeep } from 'jest-mock-extended';
import moment from 'moment';

type Vote = React.ComponentProps<typeof VotesTable>['votes'][number];

describe(TestVariant.Component, () => {
  describe(VotesTable, () => {
    const votes = [
      mockDeep<Vote>({
        account: '0x00001',
        hash: '0xH00001',
        memo: 'MIP1',
        height: 1,
        timestamp: 1_000_000_000,
        status: 'Canonical',
        direction: 'FOR',
      }),
      mockDeep<Vote>({
        account: '0x00002',
        hash: '0xH00002',
        memo: 'no MIP1',
        height: 2,
        timestamp: 2_000_000_000,
        status: 'Pending',
        direction: 'AGAINST',
      }),
    ];

    beforeEach(() => {
      render(<VotesTable votes={votes} />);
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.getByRole('button', { name: /Status/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Vote/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Download/i })).toBeVisible();

      votes.forEach((item) => {
        expect(screen.getByText(item.height)).toBeVisible();
        expect(screen.getByText(item.hash)).toBeVisible();
        expect(screen.getByText(item.status)).toBeVisible();

        const prefix = item.account.slice(0, 4);
        const suffix = item.account.slice(-4);

        expect(screen.getByText(`${prefix}...${suffix}`)).toBeVisible();

        if (item.direction === 'FOR') {
          expect(screen.getByText('FOR')).toBeVisible();
        } else {
          expect(screen.getByText('AGAINST')).toBeVisible();
        }

        const format = 'YYYY-MM-DD - hh:mm';
        const date = moment(new Date(item.timestamp)).utc();
        expect(screen.getByText(date.format(format).toString())).toBeVisible();
      });
    });
  });
});
