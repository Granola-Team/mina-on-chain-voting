import { render, screen } from 'common/test';

import { VotingPeriod } from './VotingPeriod';

describe('V1', () => {
  describe(VotingPeriod, () => {
    it('renders default state', () => {
      render(<VotingPeriod startTime={1} endTime={20} />);
      expect(screen.getByText('Voting Period')).toBeInTheDocument();
      expect(screen.getByText('Start Date')).toBeInTheDocument();
      expect(screen.getByText('End Date')).toBeInTheDocument();
    });
  });
});
