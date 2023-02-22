import { render, screen } from 'common/test';

import { VotesTable } from './VotesTable';

describe('V1', () => {
  describe(VotesTable, () => {
    it('renders default state', () => {
      render(<VotesTable votes={[]} />);
      expect(screen.getByText('Blockheight')).toBeInTheDocument();
      expect(screen.getByText('Transaction Hash')).toBeInTheDocument();
      expect(screen.getByText('Voting Status')).toBeInTheDocument();
    });
  });
});
