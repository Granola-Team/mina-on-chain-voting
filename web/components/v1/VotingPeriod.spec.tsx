import { render, screen } from 'common/test';

import { VotingPeriod } from './VotingPeriod';

describe('V1', () => {
  describe(VotingPeriod, () => {
    it('renders default state', () => {
      render(<VotingPeriod startSlot={1n} endSlot={20n} currentSlot={10n} />);
      expect(screen.getByText('Voting Period')).toBeInTheDocument();
      expect(screen.getByText('Start Slot')).toBeInTheDocument();
      expect(screen.getByText('End Slot')).toBeInTheDocument();
    });
  });
});
