import { render, screen } from 'common/test';

import { VotingResults } from './VotingResults';

describe('V1', () => {
  describe(VotingResults, () => {
    it('renders VotingResults default state', () => {
      render(<VotingResults positivePercentage={undefined} negativePercentage={undefined} />);
      expect(screen.getByText('Voting Results')).toBeInTheDocument();
      expect(screen.getByText('Yes')).toBeInTheDocument();
      expect(screen.getByText('No')).toBeInTheDocument();
    });
  });
});