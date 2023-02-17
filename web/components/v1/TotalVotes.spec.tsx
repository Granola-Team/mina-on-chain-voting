import { render, screen } from 'common/test';

import { TotalVotes } from './TotalVotes';

describe('V1', () => {
  describe(TotalVotes, () => {
    it('renders TotalVotes default state', () => {
      render(<TotalVotes totalVotes={0} />);
      expect(screen.getByText('Total Votes')).toBeInTheDocument();
    });
  });
});