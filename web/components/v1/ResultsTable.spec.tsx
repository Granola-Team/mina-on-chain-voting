import { render, screen } from 'common/test';

import { ResultsTable } from './ResultsTable';

describe('V1', () => {
  describe(ResultsTable, () => {
    it('renders default state', () => {
      const createPercentMock = jest.fn();
      render(<ResultsTable votes={[]} createPercent={createPercentMock} />);
      expect(screen.getByText('Blockheight')).toBeInTheDocument();
      expect(screen.getByText('Weighted Stake %')).toBeInTheDocument();
      expect(screen.getByText('Voting Status')).toBeInTheDocument();
    });
  });
});
