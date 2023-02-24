import { render, screen } from 'common/test';

import { VotingResults } from './VotingResults';

describe('V1', () => {
  describe(VotingResults, () => {
    it('renders default state', () => {
      const positive = 10;
      const negative = 15;
      const total = 25;

      render(<VotingResults {...{ total, positive, negative }} />);
      expect(screen.getByText('Voting Results')).toBeInTheDocument();
      expect(screen.getByText('Yes')).toBeInTheDocument();
      expect(screen.getByText('40.00%')).toBeInTheDocument();
      expect(screen.getByText('No')).toBeInTheDocument();
      expect(screen.getByText('60.00%')).toBeInTheDocument();
    });
  });
});
