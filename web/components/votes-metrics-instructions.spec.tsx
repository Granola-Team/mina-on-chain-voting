import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsInstructions } from 'components/votes-metrics-instructions';

describe(TestVariant.Component, () => {
  describe(VotesMetricsInstructions, () => {
    let user: ReturnType<typeof render>['user'];
    const memo = 'MIP1';

    beforeEach(() => {
      const result = render(<VotesMetricsInstructions memo={memo} />);
      user = result.user;
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.getByText('How do I cast my vote?')).toBeInTheDocument();
      expect(screen.getByText('I want to vote...')).toBeInTheDocument();
      expect(screen.getByText('For')).toBeInTheDocument();
      expect(screen.getByText('Against')).toBeInTheDocument();
    });

    it('toggles dialog and copies memo to clipboard', async () => {
      const writeTextMock = jest.fn();

      Object.defineProperty(navigator, 'clipboard', {
        value: { writeText: writeTextMock },
      });

      const forButton = screen.getByText('For');
      expect(forButton).toBeInTheDocument();

      await user.click(forButton);
      expect(
        screen.getByText(
          'To cast your vote you need to send yourself a transaction with the following keyword(s) in the memo field:'
        )
      ).toBeInTheDocument();

      const copyButtonText = screen.getByText(memo);
      const copyButton = copyButtonText.closest('button');
      expect(copyButton).toBeInTheDocument();

      if (copyButton) {
        await user.click(copyButton);
        expect(writeTextMock).toHaveBeenCalledWith(memo);
      } else {
        fail('Copy button should be rendered');
      }
    });
  });
});
