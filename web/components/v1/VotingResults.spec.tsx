import { render, screen } from 'common/test';

import { VotingResults } from './VotingResults';

describe('V1', () => {
  describe(VotingResults, () => {
    it('renders VotingResults default state', () => {
      const positivePercentage = "75";
      const negativePercentage = "25";
      render(<VotingResults
        positivePercentage={positivePercentage}
        negativePercentage={negativePercentage}
      />);
      expect(screen.getByText('Voting Results')).toBeInTheDocument();
      expect(screen.getByText('Yes')).toBeInTheDocument();
      expect(screen.getByText(`${positivePercentage}%`)).toBeInTheDocument();
      expect(screen.getByText('No')).toBeInTheDocument();
      expect(screen.getByText(`${negativePercentage}%`)).toBeInTheDocument();
    });
  });
});