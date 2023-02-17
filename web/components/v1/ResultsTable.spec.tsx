import { render, screen } from 'common/test';

import { ResultsTable } from './ResultsTable';

describe('V1', () => {
  describe(ResultsTable, () => {
    it('renders ResultsTable default state', () => {
      render(<ResultsTable votes={[]} createPercent={function (): void {
          throw new Error('Function not implemented.');
      } } />);
      expect(screen.getByText('Blockheight')).toBeInTheDocument();
      expect(screen.getByText('Weighted Stake %')).toBeInTheDocument();
      expect(screen.getByText('Voting Status')).toBeInTheDocument();
    });
  });
});