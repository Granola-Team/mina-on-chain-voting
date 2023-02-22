import { render, screen } from 'common/test';

import { VotingPeriod } from './VotingPeriod';

describe('V1', () => {
  describe(VotingPeriod, () => {
    it('renders default state', () => {
      const startDate = new Date('2023-02-10T10:00:00Z');
      const endDate = new Date('2023-02-17T10:00:00Z');
      const queryDate = new Date('2023-02-13T10:00:00Z');
      render(<VotingPeriod startDate={startDate} endDate={endDate} queryDate={queryDate} />);
      expect(screen.getByText('Voting Period')).toBeInTheDocument();
      expect(screen.getByText('Start Date')).toBeInTheDocument();
      expect(screen.getByText('End Date')).toBeInTheDocument();
    });
  });
});
