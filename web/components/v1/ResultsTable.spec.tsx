import { render, screen } from 'common/test';

import { ResultsTable } from './ResultsTable';

describe('V1', () => {
  describe(ResultsTable, () => {
    it('renders default state', () => {
      render(<ResultsTable votes={[]} totalStakeWeight={1000} />);
      expect(screen.getByText('Blockheight')).toBeInTheDocument();
      expect(screen.getByText('Weighted Stake %')).toBeInTheDocument();
      expect(screen.getByText('Voting Status')).toBeInTheDocument();
    });
  });
});
