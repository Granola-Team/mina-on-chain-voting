import { cleanup, render, screen, TestVariant } from 'common/test';

import { ProposalTableToolbar } from 'components/proposal-table-toolbar';

describe(TestVariant.Component, () => {
  describe(ProposalTableToolbar, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let table: any;

    beforeEach(() => {
      table = {
        getState: () => ({ columnFilters: [] }),
        getColumn: jest.fn((columnId: string) => {
          if (columnId === 'title') {
            return { getFilterValue: jest.fn(() => ''), setFilterValue: jest.fn() };
          }
          return null;
        }),
      };

      render(<ProposalTableToolbar table={table} />);
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.getByPlaceholderText('Find proposal...')).toBeVisible();
      expect(screen.getByRole('button', { name: /Status/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Category/i })).toBeVisible();
      expect(screen.getByRole('button', { name: /Download/i })).toBeVisible();
      expect(screen.queryByRole('button', { name: /Reset/i })).not.toBeInTheDocument();
    });
  });
});
